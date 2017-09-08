<%@page import="java.util.List"%>
<%@page import="org.jblooming.oql.OqlQuery"%>
<%@page import="org.jblooming.company.DepartmentType"%>
<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.ResourceBricks, com.twproject.resource.businessLogic.ResourceController, com.twproject.security.TeamworkPermissions,
 com.twproject.waf.TeamworkPopUpScreen, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Fields, org.jblooming.waf.constants.OperatorConstants,
 org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.html.input.CheckField"
  %><%
  PageState pageState = PageState.getCurrentPageState(request);


  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new ResourceController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    pageState.toHtml(pageContext);

  } else {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    String hql = "from " + DepartmentType.class.getName() + " as tt where tt.stringValue='DEPARTMENT' ";
    OqlQuery oql = new OqlQuery(hql);
    List<DepartmentType> tts = oql.list();
    String passedName=JSP.w(pageState.getEntry("name").stringValueNullIfEmpty());
%>

<style>

  .ganttAddResource tr.isCompany {
    display: none;
  }

  .ganttAddResource.isCompany tr.isCompany {
    display: table-row;
  }

  .ganttAddResource.isCompany .isPerson,
  .ganttAddResource.isCompany .isPerson {
    display: none;
  }
</style>

<div class="ganttAddResource isCompany">
  <h2><%=I18n.get("GANTT_CREATE_SUPPLIER")%>
  </h2>
  <table border="0" cellspacing="0" cellpadding="3" id="">
    <tr>
      <td colspan="4">
        <input id="isCompanyID" type="hidden" name="RESOURCE_TYPE" value="isCompany"> <%=I18n.get("ADD_SUPPLIER")%>
        <span style="float:right;" class="isPerson">
        <%
          pageState.getEntryOrDefault("CREATE_LOGIN",Fields.TRUE);
          new CheckField(I18n.get("CREATE_LOGIN"),"CREATE_LOGIN","&nbsp;",false).toHtml(pageContext);
        %>

        </span>
      </td>
    </tr>
    <tr class="isCompany"><%
    %>
      <td colspan="2"><%
    	TextField tf = new TextField(I18n.get("NAME"), "NAME", "<br>", 45, false);
        tf.required = true;
        tf.toHtml(pageContext);

      %></td>
      <td colspan="2" nowrap><%
    	//pageState.addClientEntry("TYPE", "SUPPLIER");
        if(tts!=null&&tts.size()>0){
        	pageState.addClientEntry("TYPE", tts.get(0).getId());
        }
        SmartCombo departmentType = ResourceBricks.getDepartmentTypeCombo("TYPE", pageState);
        departmentType.readOnly=true;
        departmentType.toHtml(pageContext);

      %></td>
    </tr>
    <tr>
      <td nowrap>
        <%
          tf = new TextField(I18n.get("CODE"), "CODE", "<br>", 10, false);
          tf.toHtml(pageContext);
        %>
      </td>
      <td nowrap>
        <%
          SmartCombo sb = ResourceBricks.getCompanyCombo(Fields.PARENT_ID, TeamworkPermissions.resource_canRead, null, pageState);
          sb.label = I18n.get("RESOURCE_OF");
          sb.fieldSize = 25;
          sb.separator = "<br>";
          sb.toHtml(pageContext);
        %>
      </td>
      <td><% new TextField(I18n.get("EMAIL"), "email", "<br>", 30, false).toHtml(pageContext); %></td>

    </tr>
  </table>
  <p>
    <%
      ButtonJS save = new ButtonJS(I18n.get("SAVE"), "addResource($(this));");
      save.additionalCssClass="first big";
      save.toHtml(pageContext);
    %>
  </p>

</div>


<script>


  $(function () {
    var isCompany = <%=pageState.getEntry("isCompany").checkFieldValue()%>;
        var name = "<%=passedName%>";
    var resBox=$(".ganttAddResource");
    resBox.find("[name=NAME]").val(name);
    var nameSur = name.split(" ");
    resBox.find("[name=FLD_SURNAME]").val(nameSur[0]);
    if (nameSur.length > 1) {
      nameSur.shift();
      resBox.find("[name=FLD_NAME]").val(nameSur.toString().replaceAll(",", " "));
    }
      if (isCompany)
       $("#isCompanyID").click();
  });

  function showHideResField(el) {
    //console.debug("showHideResField",el.val())
    if ("isCompany" == el.val()) {
      el.closest(".ganttAddResource").addClass("isCompany")
    } else {
      el.closest(".ganttAddResource").removeClass("isCompany")
    }
  }

  function addResourceClose(el) {
    el.closest('.ganttAddResourceBG').hide();
    event.stopPropagation()
  }

  function addResource(el) {
    var editor = el.closest(".ganttAddResource");
    if (editor.find("tr:visible :input").isFullfilled()) {
      var data = {};
      editor.find("tr:visible input").each(function () {
        data[this.name] = this.value;
      });
      data.RESOURCE_TYPE = editor.is(".isCompany") ? "COMPANY" : "PERSON";
      data.CM = "CREATERESOURCE";
      showSavingMessage();
      $.getJSON(contextPath + "/applications/teamwork/resource/resourceAjaxController.jsp", data, function (response) {
        jsonResponseHandling(response);
        if (response.ok) {
          //console.debug("about to load ",response.project);
          closeBlackPopup(response);
        }
        hideSavingMessage();
      });
    }
  }
</script>


<%
  }
%>
