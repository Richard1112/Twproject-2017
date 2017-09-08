<%@ page import="com.twproject.document.TeamworkDocument,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.html.input.UrlFileStorage,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageState,
                 org.jblooming.waf.html.button.ButtonJS" %>
<%


  PageState pageState = PageState.getCurrentPageState(request);
  if (UrlFileStorage.DRAW.equals(request.getAttribute(UrlFileStorage.ACTION))) {
    UrlFileStorage ufs = (UrlFileStorage) JspIncluderSupport.getCurrentInstance(request);
    TeamworkDocument document = (TeamworkDocument) ufs.parameters.get("document");


    // draw text field and link button
    TextField tf = new TextField(ufs.fieldName, "");
    tf.label = ufs.label;
    tf.separator = ufs.separator;
    tf.fieldSize = 40;
    tf.innerLabel = I18n.get("CHOOSE_FILE_FOLDER");
    tf.readOnly = true;
    tf.required = ufs.required;
    tf.script = "onclick=\"openFileNav('" + tf.id + "','" + document.getId() + "');\" style=\"cursor:pointer;\"";
    tf.toHtmlI18n(pageContext);

    %>&nbsp;<%

    ButtonJS buttonJS = new ButtonJS(I18n.get("EDITOR_CHOOSE"), "openFileNav('" + tf.id + "','" + document.getId() + "')");
    buttonJS.additionalCssClass = "small";
    buttonJS.toHtml(pageContext);

  }
%>