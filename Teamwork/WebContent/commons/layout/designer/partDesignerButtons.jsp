<%@ page import="org.jblooming.designer.Designer, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar,
 org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.view.PageState, org.jblooming.waf.settings.I18n"%><%

  
  Designer designer = (Designer) JspIncluderSupport.getCurrentInstance(request);
  PageState pageState = PageState.getCurrentPageState(request);


   ButtonBar bb= designer.buttonBar;

    if (!designer.readOnly) {

      ButtonSubmit saveBt = new ButtonSubmit(designer.form);
      saveBt.variationsFromForm.setCommand(Commands.SAVE);
      saveBt.label = I18n.get("SAVE");
      bb.addButton(saveBt);
    }

    bb.toHtml(pageContext);

%>