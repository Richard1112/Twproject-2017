<%@ page import="org.jblooming.ontology.IdentifiableSupport,
                 org.jblooming.ontology.LoggableIdentifiableSupport,
                 org.jblooming.persistence.PersistenceHome,
                 org.jblooming.persistence.objectEditor.FieldDrawer,
                 org.jblooming.persistence.objectEditor.FieldFeature,
                 org.jblooming.persistence.objectEditor.ObjectEditor,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.button.ButtonSupport,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.display.DeletePreviewer,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed"%>
<%@ page import="org.jblooming.waf.view.PageState, java.util.List, org.jblooming.persistence.objectEditor.businessLogic.ObjectEditorController"%>
<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8"%><%

    PageState pageState = PageState.getCurrentPageState(request);
    ObjectEditor objectEditor= (ObjectEditor)JspIncluderSupport.getCurrentInstance(request);
    IdentifiableSupport mainObject= (IdentifiableSupport) pageState.getMainObject();


    PageSeed v = objectEditor.form.url;
    v.setMainObjectId(mainObject.getId());
    v.setCommand(Commands.SAVE);

    //parametric main class handling
    v.addClientEntry("MAIN_CLASS",objectEditor.getMainObjectClass().getName());

    String parentUrl = pageState.getEntry("PARENT_URL").stringValue();
    if (parentUrl!=null)
      v.addClientEntry("PARENT_URL",parentUrl);

    String parent_class = pageState.getEntry("PARENT_CLASS").stringValue();
    if (parent_class!=null)
      v.addClientEntry("PARENT_CLASS",parent_class);

    String parent_property = pageState.getEntry("PARENT_PROPERTY").stringValue();
    if (parent_property!=null)
      v.addClientEntry("PARENT_PROPERTY",parent_property);

    String parent_id = pageState.getEntry("PARENT_ID").stringValue();
    if (parent_id!=null)
      v.addClientEntry("PARENT_ID",parent_id);

    Form form = objectEditor.form;
    if(objectEditor.isMultipart)
      form.encType = Form.MULTIPART_FORM_DATA;
    pageState.setForm(form);

    form.start(pageContext);
    %><h1><a href=""><%=objectEditor.title%></a> / <%= (JSP.ex(mainObject.getName()) ? mainObject.getName() : I18n.get("NEW"))%></h1>
<div class="container  level_2">
    <table class="table" border="0"> <%
    int numcol=3;
    int counter=0;

    boolean idDrawn = false;
    for (FieldFeature fieldFeature : objectEditor.editFields.values()){
      if (counter % numcol == 0){
        if (counter>1){
          out.println("</tr>");
        }

          if(objectEditor.idDrawn && !mainObject.isNew() && !idDrawn) {
            %><tr><td valign="top" colspan="5" align="right"><small>(id: <%=mainObject.getId()%>)</small></td></tr><%
            idDrawn = true;
            counter++;
          }
        %><tr><%
        }
      new FieldDrawer(fieldFeature,mainObject.getClass(),form).toHtml(pageContext);
      counter++;
    }
    %></tr></table><%

    ButtonBar bb= new ButtonBar();



  if (!objectEditor.readOnly) {

    ButtonSubmit saveBt = ButtonSubmit.getSaveInstance(form,I18n.get("SAVE"));
    saveBt.additionalCssClass="first big";
    if (objectEditor.windowCloseOnSubmit) {
      saveBt.additionalOnClickScript = " window.close(); ";
    }
    bb.addButton(saveBt);
  }


  /*  delete solo dalla lista
  if (!objectEditor.readOnly && !PersistenceHome.NEW_EMPTY_ID.equals(mainObject.getId()) && objectEditor.canDelete) {
    DeletePreviewer deletePreviewer = new DeletePreviewer("OBJED_DEL",objectEditor.getMainObjectClass().getName(), pageState);
    ButtonSupport deleBt = deletePreviewer.getDeleteButton(I18n.get("DELETE"), mainObject.getId() );
    deleBt.additionalCssClass="big delete";

    bb.addButton(deleBt);
    bb.addSeparator(10);
  }*/

  if (objectEditor.showDuplicateButton && !PersistenceHome.NEW_EMPTY_ID.equals(mainObject.getId()) && objectEditor.canAdd){
    ButtonSubmit duplicate = new ButtonSubmit(form);
    duplicate.variationsFromForm.setCommand(Commands.DUPLICATE );
    duplicate.label = I18n.get("DUPLICATE");
    duplicate.additionalCssClass="big";
    bb.addButton(duplicate);
  }


  if (parent_id!=null) {
    final PageSeed editParent = new PageSeed(parentUrl);
    editParent.addClientEntry("MAIN_CLASS",objectEditor.getMainObjectClass().getName());
    editParent.setCommand(Commands.EDIT);
    editParent.setMainObjectId(Integer.parseInt(parent_id));
    ButtonLink editParentL = new ButtonLink(editParent);
    editParentL.label = I18n.get("RETURN_TO_PARENT");
    editParentL.additionalCssClass="big";
    bb.addButton(editParentL);
  }

  List<ButtonSupport> buttons = objectEditor.additionalButtons;
  if(buttons!=null && buttons.size()>0) {
    for (int i = 0; i < buttons.size(); i++) {
      ButtonSupport addBut =  buttons.get(i);
      bb.addButton(addBut);
    }
  }





  if (mainObject instanceof LoggableIdentifiableSupport)
    bb.loggableIdentifiableSupport = (LoggableIdentifiableSupport)mainObject;
            
  bb.toHtml(pageContext);

  %></div><%

  form.end(pageContext);
%>