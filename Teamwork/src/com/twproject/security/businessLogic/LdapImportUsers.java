package com.twproject.security.businessLogic;

import com.opnlb.website.waf.WebSiteConstants;
import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.security.TeamworkArea;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.anagraphicalData.AnagraphicalData;
import org.jblooming.ldap.LdapUtilities;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.scheduler.Parameter;
import org.jblooming.security.LdapUser;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.settings.ApplicationState;

import java.util.Date;
import java.util.Set;
import java.util.Vector;

public class LdapImportUsers extends ExecutableSupport {
  
  @Parameter("[ldap base dn Sample value:CN=Users,DC=open-lab,DC=com]")
  public String baseDn;
  @Parameter("[ldap provider url Sample value:ldap://olfs01.open-lab.com]")
  public String providerUrl;
  @Parameter("[ldap security authentication Sample value:simple]")
  public String authentication;
  @Parameter("[ldap security principal]")
  public String principal;
  @Parameter("[ldap security credentials]")
  public String credential;
  @Parameter("[ldap filter Sample value:objectClass=user]")
  public String filter;
  @Parameter("[ldap groups Sample value:CN=Administrators,CN=Builtin,DC=open-lab,DC=com ]")
  public String ldapGroups;
  @Parameter("[ set password for import users ]")
  public String defaultPwd;
  @Parameter("[search also disabled users yes/no ]")
  public String searchDisabledUsers;
  @Parameter("[update existing users yes/no ]")
  public String updateUsers;
  @Parameter("[import additional ldap properties yes/no ]")
  public String importAdditionalProperties;


  public JobLogData run(JobLogData jobLogData) throws Exception {


    Vector<LdapUser> users = LdapUtilities.getUsers(filter,null,baseDn, LdapUtilities.getContext(providerUrl, authentication, principal, credential));
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();


      //find a default area
      String areaId = ApplicationState.getApplicationSetting(LdapUtilities.CREATE_USERS_IN_AREA)+"";
      TeamworkArea area= TeamworkArea.load(areaId);
      if (area==null)
        throw new PlatformRuntimeException("To import users from LDAP you must set an area in admin->LDAP Parameters");

      jobLogData.notes = jobLogData.notes + getClass().getName() + " executed on " + DateUtilities.dateAndHourToString(new Date());
      for (LdapUser user : users) {

        OqlQuery oql = new OqlQuery("from " + TeamworkOperator.class.getName() + " as op where lower(op.loginName) = lower(:name)");
        oql.getQuery().setString("name", user.getUsername().toLowerCase());
        TeamworkOperator operator = (TeamworkOperator) oql.uniqueResultNullIfEmpty();

        if (operator != null) {
          if (Fields.TRUE.equals(updateUsers)) {
            Person resource = (Person) PersistenceHome.findUniqueNullIfEmpty(Person.class, "myself", operator);
            Set<AnagraphicalData> ads = resource.getAnagraphicalDatas();
            if (ads.size() == 1) {
              AnagraphicalData anag = ads.iterator().next();
              if (Fields.TRUE.equals(importAdditionalProperties))
                LdapUtilities.makeAD(user, anag);
              anag.store();
              resource.store();
            }
            //set authentication to LDAP
            operator.setAuthentication("LDAP");
            operator.store();
          }
        } else {
          operator = new TeamworkOperator();
          operator.setName(user.getName());
          operator.setSurname(user.getSurname());
          operator.setLoginName(user.getUsername());
          //authentication to LDAP
          operator.setAuthentication("LDAP");
          operator.putOption(OperatorConstants.MEDIA_PREFERRED_CHANNEL, MessagingSystem.Media.LOG + "");
          operator.putOption(WebSiteConstants.HOME_PAGE, "personFirstStart.jsp");
          if (defaultPwd != null)
            operator.changePassword(defaultPwd);
          else
            operator.changePassword( user.getUsername());

          operator.store();
          AnagraphicalData anag = new AnagraphicalData();
          anag.setIdAsNew();
          anag.setOrderFactor(1);
          if (Fields.TRUE.equals(importAdditionalProperties))
            LdapUtilities.makeAD(user, anag);
          anag.setLocationDescription("-");
          anag.store();

          Person person = new Person();
          person.setIdAsNew();
          person.setPersonName(user.getName());
          person.setPersonSurname(user.getSurname());
          person.setMyself(operator);
          person.getAnagraphicalDatas().add(anag);
          person.setArea(area);
          person.setWorkDailyCapacity(CompanyCalendar.MILLIS_IN_WORKING_DAY);
          person.store();

        }
      }

      pc.commitAndClose();

    } catch (Throwable e) {
      if (pc != null)
        pc.rollbackAndClose();
      jobLogData.successfull = false;
    }
    return jobLogData;
  }
}


