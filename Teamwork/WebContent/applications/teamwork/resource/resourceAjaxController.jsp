<%@ page import="com.twproject.document.TeamworkDocument, com.twproject.document.businessLogic.DocumentAction, com.twproject.operator.TeamworkOperator, com.twproject.resource.Resource, com.twproject.security.TeamworkPermissions, net.sf.json.JSONObject, org.jblooming.anagraphicalData.AnagraphicalData, org.jblooming.messaging.Message, org.jblooming.messaging.MessagingSystem, org.jblooming.ontology.PersistentFile, org.jblooming.operator.Operator, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.JSP, org.jblooming.waf.JSONHelper, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.input.Uploader, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.Date, java.util.List, org.jblooming.waf.view.RestState, com.twproject.resource.businessLogic.ResourceAction, com.twproject.resource.Person, com.twproject.resource.Company, java.io.InputStream, java.io.ByteArrayInputStream, org.apache.tomcat.util.buf.ByteChunk, net.wimpi.pim.util.Base64, org.jblooming.utilities.StringUtilities, org.jblooming.utilities.file.FileUtilities, org.jblooming.waf.settings.Application, java.io.File, java.io.FileOutputStream, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.constants.OperatorConstants, org.jblooming.waf.constants.Fields, org.jblooming.waf.settings.I18n, org.jblooming.agenda.CompanyCalendar, org.jblooming.ontology.businessLogic.DeleteHelper, org.jblooming.system.SystemConstants, org.jblooming.waf.exceptions.ActionException" %>
<%


  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;


  try {
    if ("DELAD".equals(pageState.command)) {
      AnagraphicalData ad = AnagraphicalData.load(pageState.getEntry("idAD").intValueNoErrorCodeNoExc() + "");
      Resource resource = Resource.load(pageState.getEntry("resId").intValueNoErrorCodeNoExc() + "");
      if (resource != null && ad != null) {
        //if (resource.hasPermissionFor(logged,TeamworkPermissions.resource_canWrite) || resource.equals(logged.getPerson())) {
        if (resource.hasPermissionFor(logged,TeamworkPermissions.resource_canWrite) ) {

          if (resource.getAnagraphicalDatas().contains(ad)) { // maybe the ad is not in the resource
            resource.getAnagraphicalDatas().remove(ad);
            resource.store();
            //ad.remove();
            DeleteHelper.cmdDelete(ad,pageState);
          }
        }
      }

    } else if ("SENDNOTIFACCOUNT".equals(pageState.command)) {
      String receiver = pageState.getEntry("RECEIVER").stringValueNullIfEmpty();
      Operator toOperator = (Operator) PersistenceHome.findByPrimaryKey(Operator.class, receiver);
      Message message = new Message();
      message.setFromOperator(pageState.getLoggedOperator());
      message.setToOperator(toOperator);
      message.setDefaultExpires();
      message.setMedia(MessagingSystem.Media.EMAIL + "");
      message.setSubject(pageState.getI18n("NEW_ACCOUNT_NOTIFICATION_SBJ") + "\n");
      PageSeed ps = new PageSeed(ApplicationState.serverURL);
      ButtonLink editLink = ButtonLink.getTextualInstance(pageState.getI18n("LOGIN"), ps);
      message.setLink(editLink.toPlainLink());
      message.setMessageBody(pageState.getI18n("NEW_ACCOUNT_NOTIFICATION_BODY"));
      message.setReceived(new Date());
      message.store();

      // -------------------------------- HINTS MANAGEMENT ---------------------------
    } else if ("SKIPHINT".equals(pageState.command)) {
      String hintType = pageState.getEntry("type").stringValueNullIfEmpty();
      if (JSP.ex(hintType)) {
        logged.putOption(hintType, "yes");
        logged.store();
      }

    } else if ("SKIPALLHINTS".equals(pageState.command)) {
      logged.putOption("HINT_SKIP_ALL", "yes");
      logged.store();

    } else if ("REDOALLHINTS".equals(pageState.command)) {
      List<String> keys = new ArrayList<String>();
      keys.addAll(logged.getOptions().keySet());
      for (String key : keys) {
        if (key.startsWith("HINT_"))
          logged.getOptions().remove(key);
      }
      logged.store();


      // ---------------------------------------- DOCUMENT DROP MANAGEMENT ----------------------------------------
    } else if ("DROPDOC".equals(pageState.command)) {

      Resource resource = Resource.load(pageState.mainObjectId);
      if (resource != null && resource.hasPermissionFor(logged, TeamworkPermissions.document_canCreate)) {

        PersistentFile pf = Uploader.save(resource, "file", pageState);


        //controllare se esiste già caso mai fare una versione
        TeamworkDocument previousVersion = null;
        boolean isLocked = false;
        for (Object o : resource.getDocuments()) {
          TeamworkDocument td = (TeamworkDocument) o;
          //get last version -> root
          if (td.getParent() == null) {
            if (td.getFile() != null && pf.getOriginalFileName().equals(td.getFile().getOriginalFileName())) {
              if (td.getLockedBy() != null) {
                isLocked = true;
              }
              previousVersion = td;
              break;

            }
          }
        }

        if (!isLocked) {
          TeamworkDocument document = new TeamworkDocument();
          document.setIdAsNew();
          document.setName(pf.getOriginalFileName());
          document.setAuthor(logged.getDisplayName());
          document.setType(TeamworkDocument.IS_UPLOAD);
          document.setVersion(previousVersion == null ? "01" : previousVersion.nextVersion());
          document.setResource(resource);
          document.setFile(pf);
          document.setContent("");
          document.store();


          if (previousVersion != null) {
            previousVersion.setParentAndStore(document);
            //set references
            previousVersion.addNewVersionToReferralAndStore(document);
          }

          DocumentAction.generateDocumentEvent(document, logged);

          json.element("docMime", document.getFile().getMimeImageName());
          json.element("docHRef", document.bricks.getContentLink(pageState).getHtml());
          json.element("docId", document.getId());

        } else {
          json.element("ok", false);
          throw new Exception("File: " + pf.getOriginalFileName() + " locked by: " + previousVersion.getLockedBy().getDisplayName());
        }

      } else {
        json.element("ok", false);
        throw new Exception("No permission to create document.");
      }


      // ------------------------------------------------ CREATE A NEW RESOURCE -------------------------------------------------------------
    } else if ("CREATERESOURCE".equals(pageState.command)) {
      //chiamo la resource action con una restState in modo da poter chiamar add e poi save senza impazzire con le CE
      RestState restState = new RestState(logged);
      ResourceAction ra = new ResourceAction(restState);
      String resource_type = pageState.getEntry("RESOURCE_TYPE").stringValueNullIfEmpty();
      restState.addClientEntry("RESOURCE_TYPE", "PERSON".equals(resource_type) ? Person.class.getName() : Company.class.getName());
      restState.mainObjectId = pageState.getEntry("PARENT_ID").stringValueNullIfEmpty();
      ra.cmdAdd();

      pageState.removeEntry("RESOURCE_TYPE");// questa non contiene il nome della classe, altrimenti ammazza quella buona

      //ora si copiano i dati arrivati dall'editor sulla restState e poi si chiama il vero save
      restState.getClientEntries().addEntries(pageState.getClientEntries());

      ra.cmdSave();


      Resource newRes = (Resource) restState.getMainObject();
      if (newRes != null) {
        json.element("resId", newRes.getId());
        json.element("resName", newRes.getName());
      }

      if("PERSON".equals(resource_type) && pageState.getEntry("CREATE_LOGIN").checkFieldValue()){
        String name= restState.getEntry(OperatorConstants.FLD_NAME).stringValueNullIfEmpty();
        String surname= restState.getEntry(OperatorConstants.FLD_SURNAME).stringValueNullIfEmpty();
        String login = (JSP.ex(name) ? name.charAt(0) : "") + surname;
        String password = (JSP.ex(name) ? name : "1" + surname);

        restState.addClientEntry("LOGIN_NAME", login);
        restState.addClientEntry(OperatorConstants.FLD_IS_ENABLED, Fields.TRUE);
        restState.addClientEntry("PWD", password);
        restState.addClientEntry("PWD_RETYPE", password);
        restState.addClientEntry("PWD_RETYPE", password);

        restState.mainObjectId=restState.getMainObject()==null?"":restState.getMainObject().getId();
        ra.cmdSaveSecurity();
        json.element("loginCreatedMessage", I18n.get("RESOURCE_%%_CREATED_WITH_LOGIN_%%_AND_PASSWORD", newRes.getDisplayName(), login, password));
      }


      // ------------------------------------------------ REMOVE PROFILE IMAGE -------------------------------------------------------------
    } else if ("RMIMG".equals(pageState.command)) {
      Person person = Person.load(pageState.getEntry("RES_ID").intValueNoErrorCodeNoExc() + "");
      if (person != null && person.hasPermissionFor(logged, TeamworkPermissions.resource_canWrite) || person.equals(logged.getPerson())) {
        PersistentFile oldPhoto = person.getMyPhoto();
        if (oldPhoto!=null)
          oldPhoto.delete();

        person.setMyPhoto(null);
        person.store();
        json.element("imageUrl",person.bricks.getAvatarImageUrl());
      }
      // ------------------------------------------------ SAVE PROFILE IMAGE -------------------------------------------------------------
    } else if ("SVIMG".equals(pageState.command)) {
      Resource resource = Resource.load(pageState.getEntry("RES_ID").intValueNoErrorCodeNoExc() + "");
      String imgData = pageState.getEntry("imgData").stringValueNullIfEmpty();
      if (JSP.ex(imgData) && resource != null && resource.hasPermissionFor(logged, TeamworkPermissions.resource_canWrite) || resource.equals(logged.getPerson())) {

        String fileName = "";
        String imgType = "";

        if (imgData.toLowerCase().indexOf("base64,") > -1) {
          imgType = "." + imgData.substring(imgData.indexOf("/") + 1, imgData.indexOf(";"));
          imgData = imgData.substring(imgData.indexOf("base64,") + "base64,".length());
        }

        byte[] decode = Base64.decode(imgData.getBytes());
        InputStream in = new ByteArrayInputStream(decode);

        PersistentFile oldPhoto = resource.getMyPhoto();
        if (oldPhoto!=null)
          oldPhoto.delete();

        //no ext case
        if (!JSP.ex(imgType))
          imgType = ".jpeg";
        String imageName = StringUtilities.paddTo(resource.getIntId(), "000000");

        imageName = imageName+"_" +StringUtilities.generatePassword(5) +imgType;

        File f=new File(ApplicationState.webAppFileSystemRootPath+File.separator+"avatars");
        if (!f.exists())
          f.mkdirs();

        FileOutputStream fos= new FileOutputStream(ApplicationState.webAppFileSystemRootPath+File.separator+"avatars"+File.separator+imageName);
        FileUtilities.copy(in,fos,true);

        PersistentFile persistentFile = new PersistentFile(resource.getIntId(),imageName,PersistentFile.TYPE_WEBAPP_FILESTORAGE);
        persistentFile.setFileLocation("avatars/"+imageName);

        resource.setMyPhoto(persistentFile);
        resource.store();
        json.element("imageUrl",resource.bricks.getAvatarImageUrl());

      }


    // ------------------------------------------------ CHANGE PASSWORD-------------------------------------------------------------
    } else if ("CHANGEPWD".equals(pageState.command)) {
      ClientEntry oldPasswordCE = pageState.getEntry("oldPassword");

      //se corrisponde alla vecchia pwd
      if(logged.computePassword(JSP.w(oldPasswordCE.stringValueNullIfEmpty())).equalsIgnoreCase(logged.getPassword())){

        ClientEntry newPasswordCE= pageState.getEntry("newPassword");
        String newPassword=JSP.w(newPasswordCE.stringValueNullIfEmpty());

        // test for password sufficient length
        int minLength =  new ClientEntry("aa", ApplicationState.getApplicationSetting(SystemConstants.FLD_PASSWORD_MIN_LEN)).intValueNoErrorCodeNoExc();
        if (newPassword.length() >= minLength) {
          ClientEntry retypePasswordCE = pageState.getEntry("retypePassword");
          String retypePassword=JSP.w(retypePasswordCE.stringValueNullIfEmpty());


          // test for retyped password identical
          if (newPassword.equals(retypePassword)) {
            logged.changePassword(newPassword);
            logged.store();
          } else {
            retypePasswordCE.errorCode = "ERR_PASSWORD_MUST_BE_IDENTICAL";
          }

        } else {
          newPasswordCE.errorCode = "ERR_PASSWORD_TOO_SHORT";
        }



      } else {
        oldPasswordCE.errorCode="OLD_PASSWORD_DOES_NOT_MATCH";
      }

    }

  } catch (Throwable t) {
    jsonHelper.error(t);
  }

  jsonHelper.close(pageContext);

%>
