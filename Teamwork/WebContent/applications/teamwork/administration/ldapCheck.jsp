<%@ page import="org.jblooming.ldap.LdapUtilities, org.jblooming.tracer.Tracer, org.jblooming.waf.view.PageState, javax.naming.AuthenticationException, javax.naming.directory.DirContext" %>
<%boolean error=false;
 String errorMsg="";
 PageState pageState = PageState.getCurrentPageState(request);
 String prov_url  = pageState.getEntry(LdapUtilities.PROVIDER_URL).stringValueNullIfEmpty();
 String sec_principal = pageState.getEntry(LdapUtilities.SECURITY_PRINCIPAL).stringValueNullIfEmpty();
 String sec_auth = pageState.getEntry(LdapUtilities.SECURITY_AUTHENTICATION).stringValueNullIfEmpty();
 String sec_credentials = pageState.getEntry(LdapUtilities.SECURITY_CREDENTIALS).stringValueNullIfEmpty();

 try {
  DirContext ctx  = LdapUtilities.getContext(prov_url,sec_auth, sec_principal,sec_credentials);

 }catch(AuthenticationException auth){
    error = true;
    auth.printStackTrace();
    errorMsg = "Connection refused principal or credential invalid.";
 }catch(Throwable e){
    error = true;
    errorMsg ="Connection refused: "+e.getMessage() ;
    Tracer.platformLogger.error(e);
  }

if(! error){%>
 <br><h3>Successful !!</h3>
<%}else{%>
 <br><h3><font class="warning"><%=errorMsg%></font></h3>
<%}%>