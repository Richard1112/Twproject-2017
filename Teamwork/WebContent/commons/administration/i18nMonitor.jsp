<%@ page import="org.jblooming.operator.User, org.jblooming.security.PlatformPermissions, org.jblooming.utilities.JSP, org.jblooming.utilities.file.FileUtilities, org.jblooming.waf.ScreenBasic, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.Application, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.io.File, java.io.FileFilter, java.util.*" %>
<%!

    public static Set<File> getFilesRecursively(File root, FileFilter filter) {
    Set<File> files = new HashSet();
    if (root.exists() && root.isDirectory()) {
      getFilesRecursivelyRecur(root, files,filter);
    }
    return files;
  }

  private static void getFilesRecursivelyRecur(File file, Set<File> files,FileFilter filter) {
    if (file.isDirectory())
      for (File child : file.listFiles(filter))
        getFilesRecursivelyRecur(child, files,filter);
    else
      files.add(file);
  }


%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  pageState.getLoggedOperator().testIsAdministrator();
  pageState.setPopup(true);


  if (!pageState.screenRunning) {

    ScreenBasic.preparePage(null, pageContext);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

    User loggedUser = pageState.getLoggedOperator();
    if (loggedUser != null && loggedUser.hasPermissionFor(PlatformPermissions.i18n_manage)) {

      String pathToSourcesRoot = pageState.getEntry("SOURCES_ROOT").stringValueNullIfEmpty();
      Map<String,String> burp= new Hashtable<String, String>();

      if (JSP.ex(pathToSourcesRoot)) {

        FileFilter filter = new FileFilter() {
          public boolean accept(File file) {
            return file.isDirectory()|| file.getName().endsWith(".java") || file.getName().endsWith(".jsp");
          }
        };

        Set<File> files=getFilesRecursively(new File("c:\\develop\\java\\teamworkTrunk"), filter);

        for (File file:files){
          burp.put(file.getCanonicalPath(), FileUtilities.readTextFile(file.getCanonicalPath()));
        }
      }

      // hack to temporary disable i18nedit
      String old_status = I18n.getEditStatus();
      I18n.setEditStatus(I18n.EDIT_STATUS_READ);

      I18n i18nManager = ApplicationState.i18n;

      PageSeed tp = pageState.thisPage(request);
      tp.addClientEntry("APPNAME","");
      Form f = new Form(tp);
      f.start(pageContext);

      Container box = new Container("bx_18li");
      box.title = pageState.getI18n("I18N_MONITOR");
      box.start(pageContext);


      %> Current modality: <%= JSP.w(old_status)%> <br>

      Active languages: <br><%
        for (String lang:   i18nManager.supportedLanguages) {
              %><%=lang%>&nbsp;&nbsp;<%
        }



      %> <hr> <div id="dummy"></div>

     <br>
    <%
      TextField tf = new TextField("SOURCES_ROOT","&nbsp;");
      tf.label="File systems sources root";
      tf.toHtml(pageContext);
    %>

      Active applications: <br>
        <table>
          <tr>
            <th>Application</th>
            <th>entries</th>
            <th>seen</th>
            <th>suspect</th>
            <th colspan="2">see</th>
            <th>raz</th>

          </tr>
      <%
      for (Application app:   ApplicationState.platformConfiguration.applications.values()) {
      %>
          <tr  class="alternate" >
            <td><%=app.getName()%></td>
          <%
            int seen=0;
            int susp=0;
            Collection<I18n.I18nEntry> i18nEntries = I18n.getEntriesForApplication(app.getName()).values();
            for (I18n.I18nEntry i18ne:   i18nEntries) {
              seen+= i18ne.isSeen()?1:0;
              susp+= i18ne.isSuspect()?1:0;
            }
          %>
            <td><%=i18nEntries.size()%></td>
            <td><%=seen%></td>
            <td><%=susp%></td>

            <td><%PageSeed ps = pageState.thisPage(request);
              ps.setCommand("SEEUNSEEN");
              ps.addClientEntry("APPNAME",app.getName());
              ButtonSubmit see = new ButtonSubmit(f);
              see.variationsFromForm = ps;
              see.label="list unseen";
              see.toHtml(pageContext);%></td>

            <td><%ps = pageState.thisPage(request);
              ps.setCommand("SEESUSPECT");
              ps.addClientEntry("APPNAME",app.getName());
              see = new ButtonSubmit(f);
              see.variationsFromForm = ps;
              see.label="list suspect";
              see.toHtml(pageContext);%></td>

            <td><%ps.setCommand("REMOVE_UNSEEN");
              ps.addClientEntry("APPNAME",app.getName());
              see = new ButtonSubmit(f);
              see.variationsFromForm = ps;
              see.label="remove unseen";
              see.confirmQuestion="do you really want remove unseen for "+app.getName()+"?";
              see.confirmRequire=true;
              see.toHtml(pageContext);%></td>


        </tr><%
      }
          int seen=0;
          int susp=0;
          for (I18n.I18nEntry i18ne:   i18nManager.codeEntries.values()) {
            seen+= i18ne.isSeen()?1:0;
            susp+= i18ne.isSuspect()?1:0;
          }
      %>
          <tr>
            <td>Total</td>
            <td><%=i18nManager.codeEntries.size()%></td>
            <td><%=seen%></td>
            <td><%=susp%></td>
            <td></td>
            <td></td>
          </tr>
        </table>
      <%

        box.end(pageContext);

        // hack to temporary disable i18nedit
        I18n.setEditStatus(old_status);

        // list unseen/suspect
        boolean showUnseen = "SEEUNSEEN".equals(pageState.command);
        boolean showSuspects="SEESUSPECT".equals(pageState.command);

        if (showUnseen || showSuspects) {
          %><%=showUnseen?"Unseen list":"Suspects list"%><table><tr><th>Code</th><%
          for (String lang:   i18nManager.supportedLanguages) {
              %><th><%=lang%></th><%
          }
          %><%
              if (JSP.ex(pathToSourcesRoot)) {
                %><th>seen in files</th><%
              }
            %><th>&nbsp;</th><th>&nbsp;</th>

          </tr><%
          String applicationName = pageState.getEntry("APPNAME").stringValue();
          Collection<I18n.I18nEntry> i18nEntries = I18n.getEntriesForApplication( applicationName).values();
          int i =0;


          PageSeed deleteI18n = pageState.pagePart(request);
          deleteI18n.setCommand(Commands.DELETE);
          for (I18n.I18nEntry i18ne:   i18nEntries) {
            if ( (showUnseen && !i18ne.isSeen()) || (showSuspects && i18ne.isSuspect())){
              i++;
              %><tr  class="alternate"  id="tr_<%=i%>">
                <td><%=i18ne.getCode()%></td><%
                I18n.ApplicationEntry ae=i18ne.applicationEntries.get(applicationName);
                for (String lang:   i18nManager.supportedLanguages) {
                    %><td onclick="getI18n('<%=ae.entries.get(lang)%>');"><%=ae.entries.get(lang)%></td><%
                }


              if (JSP.ex(pathToSourcesRoot)) {
                %><td><%
                boolean notFound = true;
                for (Map.Entry<String,String> e: burp.entrySet()) {
                  if (e.getValue().contains(i18ne.getCode())) {
                    %><small>Found in <%=e.getKey()%></small><%
                      notFound = false;
                    break;
                  }
                }    
                %><%=notFound ? "<font color=red>NOT FOUND</font>":""%></td><%
              }

              %><td><span class="teamworkIcon" onClick="if (confirm('sure that it is used?')){getI18n('<%=i18ne.getCode()%>');$('#tr_<%=i%>').remove();}">;</span></td>



  <td><span class="teamworkIcon" style="cursor: pointer" onclick="deleteLabel(this,'<%=i18ne.code%>');">d</span></td></tr><%
            }
          }
          %></table> <%


      } else   if ("REMOVE_UNSEEN".equals(pageState.command)){
        String applicationName = pageState.getEntry("APPNAME").stringValue();
        Collection<I18n.I18nEntry> i18nEntries = I18n.getEntriesForApplication( applicationName).values();
        for (I18n.I18nEntry i18ne:   i18nEntries) {
          for (String lang:   i18nManager.supportedLanguages) {
            if (!i18ne.isSeen())
              i18nManager.removeEntry(i18ne.getCode(),applicationName,lang);
          }
        }


      }


      f.end(pageContext);

    }


    %><script type="text/javascript">
  function deleteLabel(el,label) {
    showSavingMessage()
    $.get("i18nAjaxController.jsp",{CM:"CMD_REMOVE_LABEL_ALL",OBJID:label},function(){
      $(el).closest('tr').remove();
      hideSavingMessage();
    });
  }

    </script><%



  }

%>