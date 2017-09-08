<%@ page import="org.jblooming.ontology.Identifiable, org.jblooming.operator.Operator, org.jblooming.page.Page, org.jblooming.persistence.objectEditor.FieldDrawer, org.jblooming.persistence.objectEditor.FieldFeature, org.jblooming.persistence.objectEditor.ObjectEditor, org.jblooming.utilities.DateUtilities,
                 org.jblooming.utilities.JSP, org.jblooming.utilities.ReflectionUtilities, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.display.Paginator, org.jblooming.waf.html.input.LoadSaveFilter, org.jblooming.waf.html.state.Form, org.jblooming.waf.html.table.ListHeader, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState, java.lang.reflect.Method, java.util.Date, java.util.List, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.display.DeletePreviewer" %><%

  PageState pageState = PageState.getCurrentPageState(request);
  Operator loggedUser = pageState.getLoggedOperator();


  ObjectEditor objectEditor = (ObjectEditor) JspIncluderSupport.getCurrentInstance(request);
  Class mainObjectClass = objectEditor.getMainObjectClass();

  PageSeed v = objectEditor.form.url;
  v.setCommand(Commands.FIND);
  //parametric main class handling
  v.addClientEntry("MAIN_CLASS", objectEditor.getMainObjectClass().getName());
  v.setMainObjectId("");
  Form form = objectEditor.form;
  pageState.setForm(form);
  form.start(pageContext);


  new DeletePreviewer("OBJED_DEL",objectEditor.getMainObjectClass().getName(), pageState);



%><h1><%=objectEditor.title%></h1><%
/*
________________________________________________________________________________________________________________________________________________________________________


search

________________________________________________________________________________________________________________________________________________________________________

*/
  Container ctSearch = new Container();
  ctSearch.level=3;
  ctSearch.start(pageContext);

%>
<table class="table" border="0">
  <tr><%

    int numcol = 2;
    int counter = 0;

    for (FieldFeature fieldFeature : objectEditor.displayFields.values()) {
      if (fieldFeature.usedForSearch) {
        if (counter % numcol == 0) {
  %></tr>
  <tr><%
        }
        FieldDrawer fd = new FieldDrawer(fieldFeature, mainObjectClass, form);
        fd.submitOnKeyReturn = true;
        fd.toHtml(pageContext);
        counter++;
      }
    }

  %></tr>
</table>
<%
  //end container


  Page objs = pageState.getPage();

  ButtonBar bb = new ButtonBar();

  ButtonSubmit bs = new ButtonSubmit(form);
  bs.label = I18n.get("SEARCH");
  bs.additionalCssClass = "first";
  bb.addButton(bs);

  if (!objectEditor.readOnly) {
    ButtonSubmit addBl = new ButtonSubmit(form);
    addBl.variationsFromForm.setCommand(Commands.ADD);
    addBl.label = I18n.get("ADD");
    if (objectEditor.canAdd)
      bb.addButton(addBl);
  }
  bb.addSeparator(10);




  // objectEditor additional buttons (thanx to RENATO2)
  List<ButtonSupport> buttons = objectEditor.additionalButtons;
  if (buttons != null && buttons.size() > 0) {
    for (int i = 0; i < buttons.size(); i++) {
      ButtonSupport addBut = buttons.get(i);
      bb.addButton(addBut);
    }
  }


  ButtonSupport qbe = ButtonLink.getBlackInstance(JSP.wHelp(I18n.get("HELP")), 700, 800, pageState.pageFromCommonsRoot("help/qbe.jsp"));
  qbe.toolTip = I18n.get("HELP_QBE");
  bb.addButton(qbe);
  bb.addSeparator(10);

  // no logged no save filter
  LoadSaveFilter lsf = null;
  if (loggedUser != null) {
    lsf = new LoadSaveFilter( "LSFOBJED" + objectEditor.getMainObjectClass().getName(), form);
    bb.addButton(lsf);
  }

  bb.toHtml(pageContext);

  ctSearch.end(pageContext);

/*
________________________________________________________________________________________________________________________________________________________________________


list

________________________________________________________________________________________________________________________________________________________________________

*/

%><%new Paginator("OBJEDPG" + objectEditor.title, form).toHtml(pageContext);%>
<table class="table fixHead fixFoot dataTable">
  <thead class="dataTableHead">
  <%

    ListHeader lh = new ListHeader("OBJEDLH" + objectEditor.title, form);
    if (objectEditor.bulkActions){
      CheckField cf = new CheckField("","chall","",false);
      cf.toolTip=I18n.get("SELECT_DESELECT_ALL");
      cf.script=" onclick=\"selUnselAll($(this));\"";
      lh.getHeaders().add(cf);
      //cf.toHtml(pageContext);
    }

    if (objectEditor.canEdit)
      lh.addHeaderFitAndCentered(I18n.get("EDIT_SHORT"));
    for (FieldFeature fieldFeature : objectEditor.displayFields.values()) {
      if (fieldFeature.blank != null)
        continue;
       if(fieldFeature.noSortable)
         lh.addHeader(fieldFeature.label);
      else
      lh.addHeader(fieldFeature.label, objectEditor.mainHqlAlias + "." + fieldFeature.propertyName);
    }

    if (!objectEditor.readOnly && objectEditor.canDelete) { // - add controls on canDelete
      lh.addHeaderFitAndCentered(I18n.get("DELETE_SHORT"));
    }
    lh.toHtml(pageContext);

    %></thead>
    <tbody><%
    if (objs != null) {
      ButtonSubmit bEdit = ButtonSubmit.getTextualInstance("", form);
      bEdit.variationsFromForm.setCommand(Commands.EDIT);
      bEdit.iconChar="e";
      bEdit.label="";

      ButtonJS bd = new ButtonJS("delRow($(this))");
      bd.iconChar="d";
      bd.label="";
      bd.additionalCssClass="delete";



      for (Object oobj : objs.getThisPageElements()) {
        Identifiable obj = (Identifiable) oobj;
        bEdit.toolTip = "" + obj.getId();
        bEdit.variationsFromForm.mainObjectId = obj.getId();

  %>
  <tr class="alternate" objId="<%=obj.getId()%>" >
    <%
      if (objectEditor.bulkActions){
        %><td align="center"><input type="checkbox" onclick="refreshBulk($(this));" class="selector"></td><%
      }

      if (objectEditor.canEdit) {
        %><td align="center"><%bEdit.toHtmlInTextOnlyModality(pageContext);%></td><%
      }
      for (FieldFeature fieldFeature : objectEditor.displayFields.values()) {
        if (fieldFeature.blank != null)
          continue;
        String display = "-";
        Object value = ReflectionUtilities.getFieldValue(fieldFeature.propertyName, obj);

        if (value != null) {
          if (value instanceof Identifiable) {
            Identifiable identifiable = ((Identifiable) value);
            Method method = ReflectionUtilities.getDeclaredInheritedMethods(identifiable.getClass()).get("getDisplayName");
            if (method != null) {
              display = (String) method.invoke(identifiable);
            } else {
              display = identifiable.getName();
            }
          } else if (value instanceof Date) {
            if (fieldFeature.format == null)
              display = DateUtilities.dateToString((Date) value);
            else
              display = DateUtilities.dateToString((Date) value, fieldFeature.format);
          } else if (value.toString().equals("true") || value.toString().equals("false")) {
            display = I18n.get(value.toString().toUpperCase());
          } else {
            display = value.toString();
          }
        }
    %>
    <td><%=display%></td>
    <%
      }

      if (!objectEditor.readOnly && objectEditor.canDelete) {
        bd.toolTip = "" + obj.getId();

    %>
    <td align="center"><%bd.toHtmlInTextOnlyModality(pageContext);%></td>
  </tr>
  <%
      }
    }
  } else {
  %>
  <tr>
    <td colspan="4" align="center"><%=I18n.get("FILTER")%>
    </td>
  </tr>
  <%
    }
  %>
</tbody><%
  if (objectEditor.bulkActions) {
  %><tfoot ><tr ><td id = "bulkPlace" colspan = "99" ></td ></tr ></tfoot ><%
    }
%>

</table><%


  if (objectEditor.bulkActions) {
    %>
    <input type="hidden" name="allIds" id="allIds">
    <div id="bulkOp" style="display:none;">
    <div id="bulkRowSel"></div>

    <div><%
    ButtonJS removeAll = new ButtonJS(I18n.get("ISSUE_REMOVE_ALL"), "removeAll($(this));");
    removeAll.confirmRequire = true;
    removeAll.iconChar = "&#xa2;";
    removeAll.confirmQuestion = I18n.get("FLD_CONFIRM_DELETE");
    removeAll.label = I18n.get("ISSUE_REMOVE_ALL");
    removeAll.toHtmlInTextOnlyModality(pageContext);
    %></div></div>
    <script type="text/javascript">
    function removeAll(el){
      var allIds=[];
      $(".selector:checked").each(function() {allIds.push($(this).closest("tr[objId]").attr("objId"));});
      $("#allIds").val(allIds.concat());
      $("[name=CM]").val("DEL_ALL");
      el.closest("form").submit();

    }

  </script>


<%
  }

  form.end(pageContext);

%>
<script>

  function delRow(el) {
    var issueRow = $(el).closest("[objid]");
    var issueId = issueRow.attr("objid");
    deletePreview("OBJED_DEL", issueId, function(response) {  // callback function
      if (response && response.ok){
        issueRow.fadeOut(500, function () {$(this).remove();});
      }
    });
  }
</script>
