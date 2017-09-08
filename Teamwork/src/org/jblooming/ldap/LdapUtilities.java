package org.jblooming.ldap;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.anagraphicalData.AnagraphicalData;
import org.jblooming.security.LdapUser;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.settings.ApplicationState;

import javax.naming.AuthenticationException;
import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.*;
import java.io.File;
import java.util.*;

public class LdapUtilities {

  //Active Directory Attributes  
  public static String USER_PRINCIPAL_NAME = "userPrincipalName";
  public static String ACCOUNTNAME = "sAMAccountName";
  public static String SID = "objectSID";
  public static String EMAIL = "mail";
  public static String DESCRIPTION = "name";
  public static String SURNAME = "sn";
  public static String FIRSTNAME = "givenName";
  public static String PHONE = "telephoneNumber";
  public static String COUNTRY = "co";
  public static String CITY = "l";
  public static String ZIP = "postalCode";
  public static String MOBILE = "mobile";
  public static String STATE = "st";
  public static String USERSEARCHFILTER = "(objectClass=user)";
  public static String GROUPSEARCHFILTER = "(objectClass=memberOf)";
  public static String GROUPATTRIBUTE = "memberOf";


  public static final String LDAP_CONFIG_FILE =  "LDAP_CONFIG_FILE";
  public static final String INITIAL_CONTEXT_FACTORY =  "LDAP_INITIAL_CONTEXT_FACTORY";
  public static final String PROVIDER_URL = "LDAP_PROVIDER_URL";
  public static final String SECURITY_AUTHENTICATION =  "LDAP_SECURITY_AUTHENTICATION";
  public static final String SECURITY_PRINCIPAL =  "LDAP_SECURITY_PRINCIPAL";
  public static final String SECURITY_CREDENTIALS =  "LDAP_SECURITY_CREDENTIALS";
  public static final String BASE_DN =  "LDAP_BASE_DN";
  public static final String DOMAIN_NAME =  "LDAP_DOMAIN_NAME";

  public static final String ENABLE_LDAP_SSL =  "LDAP_ENABLE_LDAP_SSL";


  public static final String CREATE_USERS_ON_LOGIN =  "LDAP_CREATE_USERS_ON_LOGIN";
  public static final String CREATE_USERS_IN_AREA =  "LDAP_CREATE_USERS_IN_AREA";


  //ldap account constants
  private static int UF_ACCOUNTDISABLE = 0x0002;
  private static int UF_PASSWD_NOTREQD = 0x0020;
  private static int UF_PASSWD_CANT_CHANGE = 0x0040;
  private static int UF_NORMAL_ACCOUNT = 0x0200;
  private static int UF_DONT_EXPIRE_PASSWD = 0x10000;
  private static int UF_PASSWORD_EXPIRED = 0x800000;

  public static List<String> getMemberOf(String baseDN, String cn, DirContext ctx, List<String> groups) throws NamingException {

    SearchControls ctls = new SearchControls();
    ctls.setCountLimit(1);
    ctls.setSearchScope(SearchControls.SUBTREE_SCOPE);
    ctls.setReturningAttributes(new String[]{"memberof"});

    NamingEnumeration nm = ctx.search(baseDN, cn, ctls);
    if (nm != null && nm.hasMoreElements()) {
      SearchResult sr = (SearchResult) nm.nextElement();
      Attributes atts = sr.getAttributes();
      NamingEnumeration allAttr = atts.getAll();
      if (allAttr != null && allAttr.hasMore()) {
        if (groups == null)
          groups = new ArrayList<String>();
        Attribute att = (Attribute) allAttr.next();
        NamingEnumeration attEn = att.getAll();
        while (attEn.hasMore()) {
          String groupSer = (String) attEn.next();
          String group = StringUtilities.splitToArray(groupSer, ",")[0];
          if (!groups.contains(group))
            groups.add(group);
          getMemberOf(baseDN, group, ctx, groups);
        }
      }
    }
    return groups;
  }


  public static Map<String, Set<String>> getPropertyOf(String baseDN, String cn, DirContext ctx) throws NamingException {
    SearchControls ctls = new SearchControls();
    ctls.setCountLimit(1);
    ctls.setSearchScope(SearchControls.SUBTREE_SCOPE);

    NamingEnumeration nm = ctx.search(baseDN, cn, ctls);

    Map<String, Set<String>> props = new HashMap<String, Set<String>>();

    if (nm.hasMoreElements()) {
      SearchResult sr = (SearchResult) nm.nextElement();
      props = getProperty(sr);
    }
    return props;
  }

  public static Map<String, Set<String>> getProperty(SearchResult sr) throws NamingException {
    Map<String, Set<String>> props = new HashMap<String, Set<String>>();
    Attributes atts = sr.getAttributes();
    NamingEnumeration allAttr = atts.getAll();
    while (allAttr.hasMore()) {
      Attribute att = (Attribute) allAttr.next();
      String propId = att.getID();
      Set<String> propValSet = new HashSet<String>();
      NamingEnumeration attEn = att.getAll();
      if (attEn != null && attEn.hasMore()) {
        while (attEn.hasMore()) {
          propValSet.add(attEn.next() + "");
        }
      } else {
        propValSet.add(att.get().toString());
      }
      props.put(propId, propValSet);
    }
    return props;
  }

  public static LdapUser getLdapUser(SearchResult srt) throws NamingException {
    LdapUser ldapUser = new LdapUser();

    Attributes attrs = srt.getAttributes();
    if (attrs.get(ACCOUNTNAME) != null)
      ldapUser.setUsername((String) attrs.get(ACCOUNTNAME).get());

    //if no FIRSTNAME or SURNAME set generate them from
    if (attrs.get(FIRSTNAME) != null)
      ldapUser.setName((String) attrs.get(FIRSTNAME).get());
    else
      ldapUser.setName(ldapUser.getUsername()+"-FIRSTNAME");

    if (attrs.get(SURNAME) != null)
      ldapUser.setSurname((String) attrs.get(SURNAME).get());
    else
      ldapUser.setSurname(ldapUser.getUsername()+"-SURNAME");

    if (attrs.get(EMAIL) != null)
      ldapUser.setEmail((String) attrs.get(EMAIL).get());
    if (attrs.get(STATE) != null)
      ldapUser.setState((String) attrs.get(STATE).get());
    if (attrs.get(CITY) != null)
      ldapUser.setCity((String) attrs.get(CITY).get());
    if (attrs.get(COUNTRY) != null)
      ldapUser.setCountry((String) attrs.get(COUNTRY).get());
    if (attrs.get(ZIP) != null)
      ldapUser.setZip((String) attrs.get(ZIP).get());
    if (attrs.get(PHONE) != null)
      ldapUser.setTelephone((String) attrs.get(PHONE).get());
    if (attrs.get(MOBILE) != null)
      ldapUser.setMobile((String) attrs.get(MOBILE).get());


    Long userAccountControl;
    if (attrs.get("userAccountControl") != null) {
      userAccountControl = Long.parseLong((String) attrs.get("userAccountControl").get());
      if ((userAccountControl & UF_ACCOUNTDISABLE) == UF_ACCOUNTDISABLE) {
        ldapUser.setEnabled(false);
      } else {
        ldapUser.setEnabled(true);
      }
    }
    if (attrs.get(USER_PRINCIPAL_NAME) != null)
      ldapUser.setUserPrincipalName((String) attrs.get(USER_PRINCIPAL_NAME).get());


    return ldapUser;
  }


  public static Vector<LdapUser> getLdapUsers(NamingEnumeration<SearchResult> sr) throws NamingException {
    Vector<LdapUser> ldapUsers = new Vector<LdapUser>();
    while (sr.hasMoreElements()) {
      SearchResult srt = sr.next();
      ldapUsers.add(getLdapUser(srt));
    }

    return ldapUsers;
  }

  public static boolean checkConnection() {
    try {
      DirContext ctx = getDefaultContext();
      if (ctx == null)
        return false;
    } catch (AuthenticationException auth) {
      Tracer.platformLogger.error(auth);
    } catch (NamingException ne) {
      Tracer.platformLogger.error(ne);
    }


    return true;
  }

  public static HashMap<String, String> getGroups(String basedn, String filter, DirContext ctx, int countLimit) throws NamingException {
    SearchControls ctls = new SearchControls();
    ctls.setSearchScope(SearchControls.SUBTREE_SCOPE);
    //ctls.setReturningAttributes(new String[]{"memberof"});
    ctls.setReturningAttributes(new String[]{LdapUtilities.GROUPATTRIBUTE});
    HashMap<String, String> groups = new HashMap();
    NamingEnumeration nm = ctx.search(basedn, filter, ctls);
    while (nm != null && nm.hasMoreElements()) {
      SearchResult sr = (SearchResult) nm.nextElement();
      Attributes atts = sr.getAttributes();
      Attribute att = atts.get(LdapUtilities.GROUPATTRIBUTE);
      if (att != null) {
        NamingEnumeration attEn = att.getAll();
        while (attEn.hasMore()) {
          String groupSer = (String) attEn.next();
          String group = StringUtilities.splitToArray(groupSer, ",")[0];
          if (groups.get(groupSer) == null)
            groups.put(groupSer, group);

        }
      }
    }

    return groups;
  }

  public static DirContext getContext(String provider_url, String security_auth, String security_principal, String security_credentials) throws NamingException {

    if (provider_url.toLowerCase().indexOf("ldap://") == -1 && provider_url.toLowerCase().indexOf("ldaps://") == -1)
      provider_url = "ldap://" + provider_url;

    /*
    if (security_principal.toLowerCase().indexOf("@") == -1)
      security_principal = security_principal + "@" + domain;
    */
    Hashtable env = new Hashtable();

    //ldap and SSL
    if (Fields.TRUE.equals(ApplicationState.getApplicationSetting(ENABLE_LDAP_SSL))) {

      String keystore = System.getProperty("java.home") + File.separator +"lib" + File.separator +"security"+File.separator +"cacerts";
      //System.setProperty(LDAPConstants.LDAP_SSL_TRUST_STORE,keystore);
      System.setProperty("javax.net.ssl.trustStore", keystore);
      env.put(Context.SECURITY_PROTOCOL, "ssl");
    }

    env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
    env.put(Context.PROVIDER_URL, provider_url);
    env.put(Context.SECURITY_AUTHENTICATION, security_auth);
    env.put(Context.SECURITY_PRINCIPAL, security_principal);
    env.put(Context.SECURITY_CREDENTIALS, security_credentials);
    DirContext ctx = new InitialDirContext(env);

    return ctx;
  }

  public static LdapUser getLdapUser(String loginName, DirContext dc) throws NamingException {

    LdapUser result = null;

    if (JSP.ex(loginName)) {

      SearchControls ctls = new SearchControls();
      ctls.setSearchScope(SearchControls.SUBTREE_SCOPE);

      NamingEnumeration nm = dc.search(ApplicationState.getApplicationSetting(BASE_DN), ACCOUNTNAME + "=" + loginName, ctls);

      //hack for active directory
      /*if (nm!=null && !nm.hasMore() && "administrator".equals(loginName)) {
        loginName = "Administrator";
        nm = dc.search(ApplicationState.getApplicationSetting(BASE_DN), ACCOUNTNAME + "=" + loginName, ctls);
      }*/

      if (nm != null && nm.hasMore()) {
        result = (LdapUser)getLdapUsers(nm).get(0);
      }
    }
    return result;
  }

  public static DirContext getDefaultContext() throws NamingException {

    String secCr = ApplicationState.getApplicationSetting(LdapUtilities.SECURITY_CREDENTIALS);

    if (JSP.ex(secCr)) {

      return getContext(ApplicationState.getApplicationSetting(PROVIDER_URL), ApplicationState.getApplicationSetting(SECURITY_AUTHENTICATION),
              ApplicationState.getApplicationSetting(SECURITY_PRINCIPAL), StringUtilities.decrypt(ApplicationState.getApplicationSetting(SECURITY_CREDENTIALS)));
    } else
      return null;


  }

  public static String getFilterForDisableUsers() {
    return "(!(userAccountControl:1.2.840.113556.1.4.803:=2))";
  }


  public static Vector<LdapUser> getUsers(String filterGroups, String additionalFilter, String basedn, DirContext ctx) {
    Vector<LdapUser> users = null;
    String filter = null;
    //additionalFilter = "(objectClass=*)";
    if (filterGroups != null)
      filter = filterGroups;
    if (additionalFilter != null) {
      if (filter != null && filter.length() > 0)
        filter = "(&" + additionalFilter + filter + ")";
      else
        filter = additionalFilter;
    }
    if (filter == null)
      return users;
    SearchControls sc = new SearchControls();
    sc.setSearchScope(SearchControls.SUBTREE_SCOPE);
    try {
      NamingEnumeration<SearchResult> sr = ctx.search(basedn, filter, sc);
      users = getLdapUsers(sr);
    } catch (NamingException ne) {
      Tracer.platformLogger.error(ne);
    }

    return users;


  }

  public static String checkUser(String prov_url, String domain, String sec_principal, String sec_auth, String sec_credentials) {
    String errorMsg = null;
    try {

      if (prov_url.toLowerCase().indexOf("ldap://") == -1 && prov_url.toLowerCase().indexOf("ldaps://") == -1)      
        prov_url = "ldap://" + prov_url;
      DirContext ctx = LdapUtilities.getDefaultContext();
      SearchControls sc = new SearchControls();
      sc.setSearchScope(SearchControls.SUBTREE_SCOPE);
      NamingEnumeration<SearchResult> sr = ctx.search(ApplicationState.getApplicationSetting(BASE_DN), USER_PRINCIPAL_NAME + "=" + sec_principal, sc);
      SearchResult rs = sr.next();
      if (rs != null) {// the usrname is valid and extract the CN attribute
        String cnValue = rs.getNameInNamespace();


        ctx = LdapUtilities.getContext(prov_url, sec_auth, cnValue, sec_credentials);
      }

    } catch (AuthenticationException auth) {
      Tracer.platformLogger.error(auth);
      errorMsg = "Connection Refused Principal or Credential invalid.";
    } catch (NamingException ne) {
      Tracer.platformLogger.error(ne);
      errorMsg = "Connection Refused Authentication Not Supported.";
    } catch (Throwable e) {
      Tracer.platformLogger.error(e);
      errorMsg = "Connection Refused: maybe username not found :-(";
    }
    return errorMsg;

  }

  public static void makeAD(LdapUser user, AnagraphicalData anag) {
    if (user.getEmail() != null)
      anag.setEmail(user.getEmail());
    if (user.getTelephone() != null)
      anag.setTelephone(user.getTelephone());
    if (user.getMobile() != null)
      anag.setMobile(user.getMobile());
    if (user.getCity() != null)
      anag.setCity(user.getCity());
    if (user.getCountry() != null)
      anag.setCountry(user.getCountry());
    if (user.getZip() != null)
      anag.setZip(user.getZip());
    if (user.getState() != null)
      anag.setProvince(user.getState());

  }

  public static Properties loadLdapMappingFromFile() {

    String ldapFileName = ApplicationState.getApplicationSetting(LdapUtilities.LDAP_CONFIG_FILE);
    if (!JSP.ex(ldapFileName))
      ldapFileName = "activeDirectory.properties";

    String globalPath = ApplicationState.webAppFileSystemRootPath + File.separator + "commons" + File.separator + "settings" + File.separator + "ldap" + File.separator + ldapFileName;
    HashMap<String, String> ldapSettings = new HashMap<String, String>();
    File global = new File(globalPath);
    Properties properties = null;
    if (global.exists()) {
      properties = FileUtilities.getProperties(globalPath);
    } else
      throw new PlatformRuntimeException("LDAP properties file not found: " + global.getAbsolutePath());

    for (Object name : properties.keySet()) {
      String key = (String) name;
      if (!key.startsWith("#"))
        ldapSettings.put(key, (String) properties.get(name));
    }

    //Read explicit mapping to default properties
    if (properties.getProperty("ACCOUNTNAME") != null)
      ACCOUNTNAME = properties.getProperty("ACCOUNTNAME");
    if (properties.getProperty("USERPRINCIPALNAME") != null)
      USER_PRINCIPAL_NAME = properties.getProperty("USERPRINCIPALNAME");
    if (properties.getProperty("FIRSTNAME") != null)
      FIRSTNAME = properties.getProperty("FIRSTNAME");
    if (properties.getProperty("SURNAME") != null)
      SURNAME = properties.getProperty("SURNAME");
    if (properties.getProperty("EMAIL") != null)
      EMAIL = properties.getProperty("EMAIL");
    //Ldap Additional fields
    if (properties.getProperty("PHONE") != null)
      PHONE = properties.getProperty("PHONE");
    if (properties.getProperty("MOBILE") != null)
      MOBILE = properties.getProperty("MOBILE");
    if (properties.getProperty("COUNTRY") != null)
      COUNTRY = properties.getProperty("COUNTRY");
    if (properties.getProperty("STATE") != null)
      STATE = properties.getProperty("STATE");
    if (properties.getProperty("ZIP") != null)
      ZIP = properties.getProperty("ZIP");
    if (properties.getProperty("CITY") != null)
      CITY = properties.getProperty("CITY");

    if (properties.getProperty("USERSEARCHFILTER") != null)
      USERSEARCHFILTER = properties.getProperty("USERSEARCHFILTER");
    if (properties.getProperty("GROUPSEARCHFILTER") != null)
      GROUPSEARCHFILTER = properties.getProperty("GROUPSEARCHFILTER");

    ApplicationState.applicationParameters.put("LDAP", ldapSettings);

    return properties;
  }
}
