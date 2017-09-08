<%@ page import="com.opnlb.website.news.News, com.opnlb.website.util.WebsiteUtilities, org.jblooming.ontology.PersistentFile, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.utilities.file.FileUtilities, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.display.Img, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.html.button.ButtonJS" %>
<%@ page pageEncoding="UTF-8" %>
<%

  JspHelper rowDrawer = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
  News news = (News) rowDrawer.parameters.get("ROW_OBJ");


  PageSeed edit = new PageSeed("newsEditor.jsp");


  edit.mainObjectId = news.getId();
  edit.setCommand(Commands.EDIT);

%>
<tr class="alternate" valign="top" newsid="<%=news.getId()%>">
  <td><%

    ButtonLink nome = ButtonLink.getTextualInstance(JSP.w(news.getTitle()), edit);
    nome.additionalCssClass="bolder";
    nome.toHtml(pageContext);

  %></td>
  <td align="center"><%=DateUtilities.dateToString(news.getStartingDate())%></td>
  <td align="center"><%=DateUtilities.dateToString(news.getEndingDate())%></td>
  <td align="center"><%=JSP.w(news.isVisible())%></td>
  <td><%
    PersistentFile pf = news.getImage();
    if (pf != null) {
      String extension = FileUtilities.getFileExt(pf.getOriginalFileName());
      if (FileUtilities.isImageByFileExt(extension)) {
        Img img = new Img(pf, I18n.get("IMAGE"));
        img.height = "50";
        img.script = WebsiteUtilities.showPreviewScript(pf.getFileLocation(), news.getTitle());
        img.toHtml(pageContext);
      } else if (FileUtilities.isFlashByFileExt(extension)) {
  %><%=I18n.get("FLASH_FILE")%><%
  } else if (FileUtilities.isQuickByFileExt(extension) || FileUtilities.isWindowsMovieByFileExt(extension)) {
  %><%=I18n.get("MOVIE_FILE")%><%
  } else if (FileUtilities.isDocByFileExt(extension)) {
  %><%=I18n.get("DOC_FILE")%><%
  } else {
  %><%=I18n.get("NO_PREVIEW")%><%
      }
    }
  %></td><%

    ButtonJS delLink = new ButtonJS("delRow($(this))");
    delLink.iconChar = "d";
    delLink.label = "";
    delLink.additionalCssClass = "delete";
    delLink.toolTip = I18n.get("DELETE") + " id: " + news.getId() + " - " + I18n.get("NAME") + ": " + news.getTitle();

  %>
  <td align="center"><%delLink.toHtmlInTextOnlyModality(pageContext);%></td>
</tr>
