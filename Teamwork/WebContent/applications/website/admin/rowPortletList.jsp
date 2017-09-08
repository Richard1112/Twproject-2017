<%@ page import=" com.opnlb.website.portlet.Portlet, org.jblooming.ontology.PersistentFile, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.button.ButtonSupport" %>
<%@page pageEncoding="UTF-8" %>
<%
  JspHelper rowDrawer = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
  Portlet wp = (Portlet) rowDrawer.parameters.get("ROW_OBJ");

  PageState pageState = PageState.getCurrentPageState(request);

  PageSeed edit = new PageSeed("portletEditor.jsp");
  edit.setCommand(Commands.EDIT);
  edit.mainObjectId = wp.getId();

%>
<tr class="alternate" valign="top">
  <td nowrap><%
    ButtonLink name = ButtonLink.getTextualInstance(wp.getName(), edit);
    name.toHtmlInTextOnlyModality(pageContext);

    PersistentFile wpPFile = wp.getFile();
  %></td>
  <td><%=JSP.w(wp.getDescription())%></td>
  <td align="center"><%=JSP.w(wp.getPixelWidth())%></td>
  <td align="center"><%=JSP.w(wp.getPixelHeight())%></td>
  <td style="word-break: break-all; color: <%=wpPFile.exists()?"black":"red"%>"><%=(wpPFile.exists() ? "" : "missing: ") + JSP.w(wpPFile).replaceAll("/", "\\\\")%></td>
  <td align="center"><%=Portlet.getUsesInPage(wp).size()%></td>
  <%

    ButtonSupport delLink=DeletePreviewer.getDeleteButton("PORT_DEL","",wp.getId());
    delLink.iconChar = "d";
    delLink.toolTip = I18n.get("DELETE") + " id: " + wp.getId() + " - " + I18n.get("NAME") + ": " + wp.getName();


    PageSeed install = pageState.thisPage(request);
    install.setCommand(Commands.INSTALL);
    install.setMainObjectId(wp.getId());
    ButtonLink inst = new ButtonLink(install);
    inst.iconChar = "&iexcl;";
    inst.label = "";
    inst.toolTip = I18n.get("INSTALLED") + " id: " + wp.getId() + " - " + I18n.get("NAME") + ": " + wp.getName();

    PageSeed uninstall = pageState.thisPage(request);
    uninstall.setCommand(Commands.UNINSTALL);
    uninstall.setMainObjectId(wp.getId());
    ButtonLink uninst = new ButtonLink(uninstall);
    uninst.iconChar = ";";
    uninst.label = "";
    uninst.toolTip = I18n.get("UNINSTALL") + " id: " + wp.getId() + " - " + I18n.get("NAME") + ": " + wp.getName();

    boolean installed = true;
    if (!wp.getInstalled())
      installed = false;

  %><td align="center"><%
    if (installed)
      uninst.toHtmlInTextOnlyModality(pageContext);
    else
      inst.toHtmlInTextOnlyModality(pageContext);

  %></td>
  <td align="center"><%delLink.toHtmlInTextOnlyModality(pageContext);%></td>
</tr>
