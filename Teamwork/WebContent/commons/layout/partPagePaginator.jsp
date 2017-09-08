<%@ page import =" org.jblooming.page.Page,
                    org.jblooming.waf.html.button.ButtonSubmit,
                    org.jblooming.waf.html.core.JspIncluderSupport,
                    org.jblooming.waf.html.display.Paginator,
                    org.jblooming.waf.html.input.TextField,
                    org.jblooming.waf.html.state.Form,
                    org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState"%><%

  PageState pageState = PageState.getCurrentPageState(request);
  Paginator paginator = (Paginator) JspIncluderSupport.getCurrentInstance(request);
  Page dataPage = pageState.getPage();
  Form f = paginator.form;

  if (dataPage != null) {
    int quantPagesShow = 10;

    ButtonSubmit button = new ButtonSubmit(f);
    button.label="";

    int minPage = 0;
    int maxPage = 0;

    int lastPageNumber = dataPage.getLastPageNumber();
    int currentPageNumber = dataPage.getPageNumber();
    if (currentPageNumber > lastPageNumber - quantPagesShow / 2)
      minPage = lastPageNumber - quantPagesShow;
    else
      minPage = currentPageNumber - quantPagesShow / 2;

    if (minPage <= 0)
      minPage = 0;

    maxPage = minPage + quantPagesShow;
    if (maxPage >= lastPageNumber)
      maxPage = lastPageNumber;


    if (Paginator.INITIALIZE.equals(request.getAttribute(Paginator.ACTION))) {

    pageState.addClientEntry(Paginator.FLD_PAGE_NUMBER, currentPageNumber + 1);
    TextField tf = new TextField(Paginator.FLD_PAGE_NUMBER, "");
    tf.label="";
    tf.id=paginator.id+"tf";
    tf.type = "hidden";
    tf.preserveOldValue = false;
    tf.toHtml(pageContext);
    } else {
      %><div class="paginator"><div class="datatableInfo"><%


      int totalNumberOfElements = dataPage.getTotalNumberOfElements();

    %><span style="color:#617777; font-style: italic;"><%=totalNumberOfElements%>&nbsp;<%=totalNumberOfElements==1?I18n.get("OBJECT_FOUND"):I18n.get("OBJECTS_FOUND")%>.</span>&nbsp;&nbsp;&nbsp;<%

  if (dataPage.getTotalNumberOfElements()==0) {
    %><span style="color:#617777; font-style: italic;"><%=I18n.get("NO_RESULTS_HELP")%></span>&nbsp;&nbsp;&nbsp;<%
  }


  if (!paginator.parameters.containsKey("HIDETOOL")){

%>&nbsp;&nbsp;<span class="teamworkIcon" style="opacity:0.4;cursor:pointer;" onclick="$(this).toggle();$('#pagSize').toggle();$('#<%=Paginator.FLD_PAGE_SIZE%>').focus();" title="<%=I18n.get("CHANGE_PAGE_SIZE")%>">g</span><span id="pagSize" style="display:none; color: #617777;"><%
  pageState.addClientEntry(Paginator.FLD_PAGE_SIZE, dataPage.getPageSize());
  TextField psize = TextField.getIntegerInstance(Paginator.FLD_PAGE_SIZE);
  psize.label=I18n.get("OP_PAGE_SIZE");
  psize.separator= "&nbsp;";
  psize.fieldSize=4;
  psize.fieldClass = "formElements formElementsSmall";
  psize.preserveOldValue=  false;
  psize.script="onBlur=\"$('[name="+Paginator.FLD_PAGE_NUMBER+"]').val(0);\"";
  psize.addKeyPressControl(13, "this.blur();obj('" + pageState.getForm().getUniqueName() + "').submit();", "onkeyup");
  psize.toHtml(pageContext);

%></span><%
  }

%></div><%
  if (lastPageNumber > 0) {


    if (dataPage.hasPreviousPage()) {
      button.additionalOnClickScript = "obj('" + paginator.id+"tf" + "').value='" + (currentPageNumber) + "';";
      button.iconChar = "{";
      button.toHtmlInTextOnlyModality(pageContext);
    }


    if (minPage > 1) {
      button.additionalOnClickScript = "obj('" + paginator.id+"tf" + "').value='" + (1) + "';";
      button.iconChar = null;
      button.label = "1...";
      button.toHtmlInTextOnlyModality(pageContext);
      button.label = "";
    }


    for (int i = minPage; i <= maxPage; i++) {
      button.additionalOnClickScript = "obj('" + paginator.id+"tf" + "').value='" + (i + 1) + "';";
      button.iconChar = null;
      button.label = "&nbsp;" + (i + 1) + "&nbsp;";
      button.hasFocus = i == currentPageNumber;
      button.toHtmlInTextOnlyModality(pageContext);
    }
    button.label = "";


    if (maxPage < lastPageNumber) {
      button.additionalOnClickScript = "obj('" + paginator.id+"tf" + "').value='" + (lastPageNumber + 1) + "';";
      button.iconChar = "";
      button.label = "..." + (lastPageNumber + 1) + "&nbsp;";
      button.toHtmlInTextOnlyModality(pageContext);
      button.label = "";
    }

    if (dataPage.hasNextPage()) {
      button.additionalOnClickScript = "obj('" + paginator.id+"tf" + "').value='" + (currentPageNumber + 2) + "';";
      button.iconChar = "}";
      button.toHtmlInTextOnlyModality(pageContext);
    }
  }

    %></div><%
  }
  }
%>
