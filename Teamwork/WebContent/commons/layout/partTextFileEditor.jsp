<%@ page import="org.jblooming.waf.SessionState,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.input.TextArea,
                 org.jblooming.waf.html.input.TextFileEditor,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState"%>
<%
  TextFileEditor tfe = (TextFileEditor)JspIncluderSupport.getCurrentInstance(request);

  PageState pageState = PageState.getCurrentPageState(request);
  tfe.perform(request, response);


  // if not defined creates a default container
  if (tfe.container==null){
    tfe.container=new Container();
    tfe.container.title = tfe.label;
  }

    TextArea ta= new TextArea("", tfe.name, "",tfe.cols,tfe.rows,"");
    Form form= tfe.form;
    boolean localForm = tfe.form == null;
    if (localForm) {
      final PageSeed iddo = pageState.thisPage(request);
      form=new Form(iddo);
      form.start(pageContext);
    } else
      form = tfe.form;

    ButtonSubmit save=new ButtonSubmit(form);
    save.variationsFromForm.setCommand(Commands.SAVE);
    save.label=pageState.getI18n("SAVE");

    tfe.container.start(pageContext);

    %> <p align="center"><%ta.toHtml(pageContext);%></p> <%

  ButtonBar butBar= tfe.bb;
  butBar.addButton(save);
  butBar.toHtml(pageContext);

  if (localForm)
    form.end(pageContext);

  tfe.container.end(pageContext);

%>