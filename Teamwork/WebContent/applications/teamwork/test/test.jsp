<%@ page import="com.twproject.waf.TeamworkHBFScreen, org.jblooming.waf.ScreenArea, org.jblooming.waf.view.PageState, com.twproject.task.Task, net.sf.json.JSONObject, com.twproject.task.businessLogic.TaskGanttAction, org.jblooming.tracer.Tracer, org.jblooming.security.PermissionCache, org.jblooming.utilities.JSP, org.jblooming.agenda.CompanyCalendar, com.twproject.resource.ResourceBricks, com.twproject.resource.Resource, java.util.List, org.jblooming.waf.view.RestState, org.jblooming.waf.html.layout.HtmlColors, java.security.KeyStore, sun.security.x509.CertAndKeyGen, java.security.cert.X509Certificate, sun.security.x509.X500Name, java.io.FileInputStream, java.io.File, java.util.ArrayList, java.util.Enumeration, java.io.FileOutputStream"
  %>

<%
  PageState pageState = PageState.getCurrentPageState(request);
  pageState.setPopup(true);
  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {

    String keyStorePwd = "ksksks";
    String keyStoreFile = "C:\\develop\\java\\Teamwork6\\support\\https\\tomcat.keystore";
    String certAlias = "pippo";

    //KeyStore keyStore = KeyStore.getInstance("PKCS12");
    KeyStore keyStore = KeyStore.getInstance("JKS");
    FileInputStream fis = new FileInputStream(new File(keyStoreFile));
    keyStore.load(fis, keyStorePwd.toCharArray());
    fis.close();

    Enumeration<String> aliases = keyStore.aliases();

    /**
     generate the certificate
     first parameter  = Algorithm
     second parameter = signrature algorithm
     third parameter  = the provider to use to generate the keys (may be null or
     use the constructor without provider)
     */

    CertAndKeyGen certGen = new CertAndKeyGen("RSA", "SHA256WithRSA", null);
    // generate it with 2048 bits
    certGen.generate(2048);

    // prepare the validity of the certificate
    long validSecs = (long) 365 * 24 * 60 * 60; // valid for one year

    // enter your details according to your application
    X500Name x500Name = new X500Name("CN=olpc009.open-lab.com,O=My Organisation,L=My City,C=DE");

    // add the certificate information, currently only valid for one year.
    X509Certificate cert = certGen.getSelfCertificate(x500Name, validSecs);

    // set the certificate and the key in the keystore
    keyStore.setKeyEntry(certAlias, certGen.getPrivateKey(), keyStorePwd.toCharArray(),new X509Certificate[]{cert});


    FileOutputStream fos = new FileOutputStream(new File(keyStoreFile));

    keyStore.store(fos,keyStorePwd.toCharArray());
    fos.close();

    while (aliases.hasMoreElements()){
      String alias = aliases.nextElement();
%><%=alias%><br><%

    }


  }
%>