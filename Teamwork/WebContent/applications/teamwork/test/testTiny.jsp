<%@ page import="com.twproject.operator.TeamworkOperator,
                 org.jblooming.waf.ScreenBasic,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.input.TinyMCE,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.view.PageState, org.jblooming.waf.settings.I18n" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  pageState.getLoggedOperator().testIsAdministrator();

  if (!pageState.screenRunning) {
    ScreenBasic.preparePage(pageContext);


    pageState.perform(request, response).toHtml(pageContext);

  } else {


    Form form = new Form(pageState.thisPage(request));
    form.alertOnChange = true;
    form.start(pageContext);

    TeamworkOperator loggedOperator = (TeamworkOperator) pageState.getLoggedOperator();


%>
<table width="100%">
  <tr>
    <td>
      <%
        TinyMCE tinyMinute = new TinyMCE("", "MEETING_MINUTE", "", "60%", "260px", pageState);
        pageState.addClientEntry("MEETING_MINUTE", "valore da testare\na capo &lt; cappello &gt; mento e <b>poi</b>  un <br /> a capo 2");
        tinyMinute.resize = true;
        tinyMinute.textArea.preserveOldValue = true;

        tinyMinute.useTinyCustomSetupJSFunction=true;
        tinyMinute.addParameter("plugins", "fullscreen");
        tinyMinute.addParameter("theme_advanced_buttons1", "code,fullscreen,separator,addIssueButton");
        tinyMinute.toHtml(pageContext);
      %>
    </td>
  </tr>
  <tr>
    <td>
      <%
        TinyMCE tinyMinute2 = new TinyMCE("", "MEETING_MINUTE2", "", "60%","260px", pageState);
        pageState.addClientEntry("MEETING_MINUTE2", "valasdfsadfsfas asdf dsf saf apo 2");
        tinyMinute2.resize = true;
        tinyMinute2.setTheme(TinyMCE.THEME_ADVANCED);
        tinyMinute2.textArea.preserveOldValue = true;

        tinyMinute2.useTinyCustomSetupJSFunction=true;
        tinyMinute2.addParameter("plugins", "fullscreen");
        tinyMinute2.addParameter("theme_advanced_buttons1", "code,fullscreen,separator,addIssueButton");
        tinyMinute2.toHtml(pageContext);
      %>
    </td>
  </tr>
  <tr>
    <td>
      <%
        ButtonBar bbM = new org.jblooming.waf.html.container.ButtonBar();
        ButtonJS js = new ButtonJS("saveMinute();");
        js.label = "save";
        bbM.addButton(js);

        ButtonJS j = new ButtonJS("test();");
        j.label = "test";
        bbM.addButton(j);



        bbM.toHtml(pageContext);
      %>
    </td>
  </tr>
</table>
<script type="text/javascript">

  function test() {
    alert(tinyMCE.activeEditor.isDirty());
    //    console.debug(tinyMCE.activeEditor.startContent);
    //console.debug(tinyMCE.activeEditor.isDirty());
    tinyMCE.activeEditor.save();
    //console.debug($("#MEETING_MINUTE").val());
    //console.debug(tinyMCE.activeEditor.isDirty());
    //console.debug($("#MEETING_MINUTE").get(0).value);
    //    console.debug(tinyMCE.activeEditor.getBody().innerHTML);
  }

  function saveMinute() {
    tinyMCE.activeEditor.save();
    $("#MEETING_MINUTE").updateOldValue();
  }


  function tinyCustomSetup(ed) {
    // Register example button

    ed.addButton('addIssueButton', {
      title : "<%=I18n.get("CREATE_ISSUE")%>",
      image : contextPath + '/img/issuesIcon.png',
      onclick : function() {
        createIssue(ed.selection.getContent());
      }
    });


      ed.onNodeChange.add(function(ed, cm, e) {
          // Activates the link button when the caret is placed in a anchor element
          if (ed.selection.getContent().trim()!="")
             cm.setDisabled('addIssueButton', false);
          else
            cm.setDisabled('addIssueButton', true);
      });
   }


  function createIssue(description){
    alert(description);
  }


</script>



</script>
<%
    form.end(pageContext);


  }%>