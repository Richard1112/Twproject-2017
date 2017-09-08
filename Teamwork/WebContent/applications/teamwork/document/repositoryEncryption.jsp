<%@ page import=" com.twproject.document.businessLogic.DocumentController,
                 com.twproject.operator.TeamworkOperator,
                 com.twproject.waf.TeamworkHBFScreen,
                 org.jblooming.ontology.PersistentFile,
                 org.jblooming.oql.OqlQuery,
                 org.jblooming.persistence.hibernate.PersistenceContext,
                 org.jblooming.remoteFile.Document,
                 org.jblooming.system.SystemConstants,
                 org.jblooming.utilities.StringUtilities,
                 org.jblooming.utilities.file.FileUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.io.*, java.util.List" %>
<%


  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new DocumentController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    logged.testIsAdministrator();

%>
<script>$("#TOOLS_MENU").addClass('selected');</script>
<%



  PageSeed self = pageState.thisPage(request);
  Form f = new Form(self);
  f.alertOnChange = false;

  pageState.setForm(f);
  f.start(pageContext);


%><h1>Crypt/De-crypt documents</h1><%


if ("CRYALL".equals(pageState.command)) {
      String repository = ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL);
      File bkFolder =  new File(repository + File.separator + "BACKUP_"+System.currentTimeMillis());
      bkFolder.mkdir();

      try{
        String hql = "select d from " + Document.class.getName() + " as d where d.persistentFile like 'FR%'";
        List<Document> docs= new OqlQuery(hql).getQuery().list();
        for (Document doc:docs ){
          PersistentFile pf = doc.getFile();
          String originalFileName = repository + File.separator + pf.getFileLocation();
          String backupFileName = bkFolder.getPath()+ pf.getFileLocation(); // senza .aes
          File oldFile = new File(originalFileName);

          %>Crypting "<%=originalFileName%>": <%


          if (oldFile.exists()){
            oldFile.renameTo(new File(backupFileName));

            oldFile=new File(backupFileName);
            File newFile= new File (originalFileName+".aes"); // encrypted files ends with .aes
            if (newFile.exists())
              newFile.delete();

            String key = I18n.isActive("CUSTOM_FEATURE_AES_CRYPTO_KEY") ? I18n.get("CUSTOM_FEATURE_AES_CRYPTO_KEY") : StringUtilities.key;

            FileInputStream fis = new FileInputStream(oldFile);
            OutputStream fos = FileUtilities.getCipherOutputStream(new FileOutputStream(newFile), key);
            FileUtilities.copy(fis,fos);

            pf.setType(PersistentFile.TYPE_ENCRYPTED_FILESTORAGE);
            doc.setFile(pf);
            doc.store();

            %> encrypted!<br><%
          } else {
            %><span style="color: red;">file not found!</span><br><%
          }

        }
        %><h3 style="color:#2dc212;">Operation completed.<br> A copy of original files has been placed here: "<%=bkFolder.getName()%>".<br> Remove it once verifyed everithing is ok.</h3><%

        } catch (Throwable t){
          PersistenceContext.getDefaultPersistenceContext().rollbackAndClose();
          PersistenceContext.getDefaultPersistenceContext();
        %><h3 style="color:#ffa308;">ERROR: Something gone wrong! Restore files from backup folder "<%=bkFolder.getName()%>".</h3><%
      }



    } else if ("DECRYALL".equals(pageState.command)) {

      String repository = ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL);
      File bkFolder =  new File(repository + File.separator + "BACKUP_"+System.currentTimeMillis());
      bkFolder.mkdir();

      try{

        String hql = "select d from " + Document.class.getName() + " as d where d.persistentFile like 'ER%'";
        List<Document> docs= new OqlQuery(hql).getQuery().list();
        for (Document doc:docs ){
          PersistentFile pf = doc.getFile();
          String originalFileName = repository + File.separator + pf.getFileLocation(); // senza aes
          String backupFileName =bkFolder.getPath()+ pf.getFileLocation()+".aes";  // i file crittati devono finire con .aes
          File oldFile = new File(originalFileName+".aes");  // i file crittati devono finire con .aes);

          %>Decrypting "<%=originalFileName%>": <%

          if (oldFile.exists()){
            oldFile.renameTo(new File(backupFileName)); // move

            oldFile=new File(backupFileName);
            File newFile= new File (originalFileName); // senza .aes
            if (newFile.exists())
              newFile.delete();

            String key = I18n.isActive("CUSTOM_FEATURE_AES_CRYPTO_KEY") ? I18n.get("CUSTOM_FEATURE_AES_CRYPTO_KEY") : StringUtilities.key;

            InputStream fis = FileUtilities.getCipherInputStream(new FileInputStream(oldFile),key);
            FileOutputStream fos = new FileOutputStream(newFile);
            FileUtilities.copy(fis,fos);

            pf.setType(PersistentFile.TYPE_FILESTORAGE);
            doc.setFile(pf);
            doc.store();

            %> decrypted!<br><%
          } else {
            %><span style="color: red;">file not found!</span><br><%
           }
          }
          %><h3 style="color:#2dc212;">Operation completed.<br> A copy of original files has been placed here: "<%=bkFolder.getName()%>".<br> Remove it once verifyed everithing is ok.</h3><%

        } catch (Throwable t){
          PersistenceContext.getDefaultPersistenceContext().rollbackAndClose();
          PersistenceContext.getDefaultPersistenceContext();
          %><h3 style="color:#ffa308;">ERROR: Something gone wrong! Restore files from backup folder "<%=bkFolder.getName()%>".</h3><%
        }


    } else {


  String hql = "select count(d.id) from " + Document.class.getName() + " as d where d.persistentFile like 'FR%'";
  long uncrypted = (Long) new OqlQuery(hql).getQuery().uniqueResult();

  hql = "select count(d.id) from " + Document.class.getName() + " as d where d.persistentFile like 'ER%'";
  long crypted = (Long) new OqlQuery(hql).getQuery().uniqueResult();


%>
<h3>Repository contains <%=uncrypted%> uncrypted files.<br>
  <%=crypted%> documents are already crypted.</h3>
<br> <br> <br>

<div style="color: red;font-size: 18px; font-weight: bolder;">
  WARNING: these operations can be disruptive!<br>
  Be sure you have a COMPLETE repository AND database backups before proceed.<br>
  Keep a copy of your cryptographic key in a safe place.<br>
  If you have doubts, DO NOTHING!


</div><br>

<h3>What do you like to do?</h3><br><big>
  <%
    if (uncrypted > 0) {
      ButtonSubmit cryall = new ButtonSubmit("Crypt " + uncrypted + " files", "CRYALL", f);
      cryall.confirmRequire = true;
      cryall.confirmQuestion = "Do you really want to crypt " + uncrypted + " files? Did you have already saved you cryptographic key?";
      cryall.toHtml(pageContext);
    }

    %>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<%
    if (crypted > 0) {
      ButtonSubmit decryall = new ButtonSubmit("De-crypt " + crypted + " files", "DECRYALL", f);
      decryall.confirmRequire = true;
      decryall.confirmQuestion = "Do you really want to de-crypt " + crypted + " files?";
      decryall.toHtml(pageContext);
    }

  %></big><br><%

    }
    f.end(pageContext);
  }
%>
