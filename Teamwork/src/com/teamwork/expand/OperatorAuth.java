package com.teamwork.expand;

import java.security.NoSuchAlgorithmException;
import java.util.Iterator;

import javax.servlet.http.HttpServletRequest;

import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.ldap.LdapUtilities;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.system.SystemConstants;
import org.jblooming.tracer.Tracer;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.settings.PlatformConfiguration;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;

public class OperatorAuth {

	   public static Boolean authPassword(PageState pageState,HttpServletRequest request) {
		   String auth_type = ApplicationState.getApplicationSetting(SystemConstants.AUTHENTICATION_TYPE);
		   Operator user=null;
		   if (SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_LDAP_AUTHENTICATION.toString().equals(auth_type)) {
		        user = ldapAuthentication(pageState);

		        // ----------------------------------------------------- LDAP_AUTHENTICATION_WITH_STANDARD_FALLBACK ----------------------------------------------------------------------------
		      } else if (SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_LDAP_AUTHENTICATION_WITH_FALLBACK_ON_STANDARD.toString().equals(auth_type)) {
		    	  try{
		    	  user = ldapAuthentication(pageState);
		    	  }catch(Exception e){
		    		  user=null;
		    	  }
		        if (user == null){
		          pageState.getEntry(OperatorConstants.FLD_LOGIN_NAME).errorCode =null;
		          pageState.getEntry(OperatorConstants.FLD_LOGIN_NEW_PWD).errorCode =null;
		          try{
		          user = standardAuthentication(pageState);
		          }catch(Exception e){
		    		  user=null;
		    	  }
		          // se l'utente è importato/aggiornato da LDAP deve autenticarsi via LDAP  se l'opzione è accesa
		          if (I18n.isActive("CUSTOM_FEATURE_FORCE_LDAP_AUTHENTICATION_FOR_IMPORTED_USERS") && user!=null && "LDAP".equals(user.getAuthentication())){
		            pageState.getEntry(OperatorConstants.FLD_LOGIN_NAME).errorCode = pageState.getI18n("ERR_INVALID_LOGIN") + " (LDAP)";
		            user=null;
		          }
		        }

		        // ----------------------------------------------------- HTTP_AUTHENTICATION ----------------------------------------------------------------------------
		      } else if (SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_HTTP_AUTHENTICATION.toString().equals(auth_type)) {
		    	  try{
		    	  user = httpAuthentication(pageState, request);
		    	  }catch(Exception e){
		    		  user=null;
		    	  }
		        // ----------------------------------------------------- STANDARD_AUTHENTICATION ----------------------------------------------------------------------------
		      } else {
		    	  try{
		    		  user = standardAuthentication(pageState);
		    	  }catch(Exception e){
		    		  user=null;
		    	  }
		        
		      }
		    return user!=null?true:false;
	   }
	   

	  public static Operator ldapAuthentication(PageState pageState) {
	    String password;
	    String username = pageState.getEntryAndSetRequired(OperatorConstants.FLD_LOGIN_NAME).stringValueNullIfEmpty();
	    password = pageState.getEntry(OperatorConstants.FLD_PWD).stringValueNullIfEmpty();
	    Operator user = null;

	    String domain = ApplicationState.getApplicationSetting(LdapUtilities.DOMAIN_NAME);
	    String provider = ApplicationState.getApplicationSetting(LdapUtilities.PROVIDER_URL);
	    String secAuth = ApplicationState.getApplicationSetting(LdapUtilities.SECURITY_AUTHENTICATION);

	    if (username != null && password != null) {
	      String msgError = LdapUtilities.checkUser(provider, domain, username, secAuth, password);
	      if (msgError != null) {
	        pageState.getEntry(OperatorConstants.FLD_LOGIN_NAME).errorCode = pageState.getI18n("ERR_INVALID_LOGIN") + " (LDAP)";

	      } else {

	        //got authorized; now search user
	        try {
	          user = Operator.findByLoginName(username);
	        } catch (PersistenceException e) {
	          Tracer.platformLogger.debug(e);
	        }
	        if (user == null) {
	          boolean create = Fields.TRUE.equals(ApplicationState.getApplicationSetting(LdapUtilities.CREATE_USERS_ON_LOGIN));
	          if (create) {
	            user = createPlatformUserFromLDAP(username, pageState);
	          } else {
	            pageState.getEntry(OperatorConstants.FLD_LOGIN_NAME).errorCode = pageState.getI18n("ERR_INVALID_LOGIN") + " (LDAP)";
	          }
	        }
	      }
	    } else {
	      pageState.getEntry(OperatorConstants.FLD_LOGIN_NAME).errorCode = pageState.getI18n("ERR_INVALID_LOGIN") + " (LDAP)";
	    }
	    return user;
	  }
	  

   protected static  Operator createPlatformUserFromLDAP(String username, PageState pageState) {
    throw new PlatformRuntimeException("LoginAction:createPlatformUserFromLDAP you must provide your implementation");
  }
   
   
   public static Operator standardAuthentication(PageState pageState) throws PersistenceException, ApplicationException {
	    String password;
	    Operator user = null;

	    try {
	      ClientEntry ceName = pageState.getEntryAndSetRequired(OperatorConstants.FLD_LOGIN_NAME);
	      ClientEntry cePassword = pageState.getEntry(OperatorConstants.FLD_PWD);

	      password = cePassword.stringValue();
	      String username = ceName.stringValue();

	      String newPassword = null;

	      try {
	        user = Operator.authenticateUser(password, username);
	      } catch (org.jblooming.security.SecurityException e) {
	        //same error to avoid user disclosure
	        ceName.errorCode = e.getMessage();
	        //ceName.errorCode = "ERR_INVALID_LOGIN";
	        Tracer.platformLogger.warn("Invalid login attempted with loginname: "+username+ " ("+e.getMessage()+")");

	      }

	      if (pageState.validEntries()) {

	        final String pass_exp = ApplicationState.getApplicationSetting(SystemConstants.FLD_PASSWORD_EXPIRY);
	        int maxDaysPassed = 0;

	        try {
	          maxDaysPassed = pass_exp != null ? Integer.parseInt(pass_exp) : 0;
	        } catch (Throwable e) {
	          Tracer.platformLogger.error("Invalid password expiry value in global settings:" + SystemConstants.FLD_PASSWORD_EXPIRY + "=" + pass_exp, e);
	        }

	        if ((maxDaysPassed > 0 &&
	                user.getLastPasswordChangeDate() != null &&
	                ((System.currentTimeMillis() - user.getLastPasswordChangeDate().getTime()) / (CompanyCalendar.MILLIS_IN_HOUR * 24)) > maxDaysPassed)) {

	          try {
	            ClientEntry ceNewPassword = pageState.getEntryAndSetRequired(OperatorConstants.FLD_LOGIN_NEW_PWD);
	            ClientEntry ceNewPasswordConfirm = pageState.getEntryAndSetRequired(OperatorConstants.FLD_LOGIN_NEW_PWD_RETYPE);

	            if (!ceNewPassword.stringValue().equals(ceNewPasswordConfirm.stringValue())) {
	              ceNewPasswordConfirm.errorCode = "ERR_PASSWORD_MUST_BE_IDENTICAL";
	              throw new ActionException();
	            }
	            final Iterator lastPasswordIterator = user.getLastPasswordIterator();
	            while (lastPasswordIterator.hasNext()) {
	              String s = (String) lastPasswordIterator.next();
	              try {
	                if (s.equals(user.computePassword(ceNewPassword.stringValue()))) {
	                  ceNewPassword.errorCode = "ERR_PASSWORD_ALREADY_USED";
	                  throw new ActionException();
	                }
	              } catch (NoSuchAlgorithmException e) {
	                throw new ApplicationException(e);
	              }
	            }

	            //passed all obstacles
	            newPassword = ceNewPassword.stringValue();

	            if (newPassword != null) {
	              user.changePassword(newPassword);
	            }


	          } catch (ActionException e) {
	          }
	        }
	      }

	    } catch (ActionException e) {
	    }
	    return user;
	  }
   
   
   public static Operator httpAuthentication(PageState pageState, HttpServletRequest request) throws PersistenceException {
	    Operator user = null;
	    if (request.getRemoteUser() != null) {
	      user = Operator.findByLoginName(request.getRemoteUser());
	    } else {
	      pageState.getEntry(OperatorConstants.FLD_LOGIN_NAME).errorCode = SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_HTTP_AUTHENTICATION + "=yes on " +
	              PlatformConfiguration.globalSettingsFileName + " but no user (request.getRemoteUser()) is provided by the web app context ";
	    }
	    return user;
	  }
}


