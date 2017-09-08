package org.jblooming.waf.html.button;

import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.html.core.UrlComposer;
import org.jblooming.waf.html.display.Img;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.ontology.LoggableIdentifiableSupport;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.security.SecurableWithArea;
import org.jblooming.security.Area;

import javax.servlet.http.HttpServletRequest;
import java.io.Serializable;

public class ButtonLink extends ButtonSupport {

  public String popup_resizable;
  public String popup_scrollbars;
  public String popup_toolbar;
  public String popup_height;
  public String popup_width;

  public PageSeed pageSeed = new PageSeed();

  //in order to generate a link without any parameter
  public boolean inhibitParams = false;

  //these useful params are for preserving from the incumbent danger of html syntax changes :-D
  @Deprecated
  public static String TARGET_BLANK = "_blank";


  public ButtonLink(PageSeed ps) {
    this(null, ps);
  }

  public ButtonLink(String label, PageSeed ps) {
    super();
    this.noPrint=false; // di solito i link devono essere stampati
    this.pageSeed = ps;
    this.label = label;
  }

  public String generateLaunchJs() {
    StringBuffer sb = new StringBuffer();
    if (enabled) {
      sb.append(" onClick=\"stopBubble(event);");
      sb.append(generateJs());
      sb.append("\" ");// close onclick string
    }
    return sb.toString();
  }

  public boolean isEnabled() {
    return enabled;
  }

  public void setMainObjectId(Serializable id) {
    pageSeed.mainObjectId=id;
  }

  public String getCEHref() {
    Link fake = new Link(pageSeed);
    fake.outputModality = UrlComposer.OUTPUT_AS_JS_LAUNCH_CES;
    return fake.getHref();
  }

  public StringBuffer generateJs() {
    StringBuffer sb = new StringBuffer();

    sb.append("try{ ");
    if (target==null)
      sb.append("window.open('");
    else
      sb.append("openCenteredWindow('");

    sb.append(pageSeed.getHref());
    if (!inhibitParams)
      sb.append('?' + getCEHref());

    sb.append("',")
            .append(target == null ? "'_self'" : '\'' + target + '\'')
            .append(",'" + getWindowFeatures() + '\'')
            .append(") } catch(e){};"); // needed for unload alert


    return sb;
  }

  public String toLink() {
    return "<a href=\"#\"" + this.generateLaunchJs() + ">" + this.label + "</a>";
  }

  public String toPlainLink() {
    StringBuffer sb = new StringBuffer();

    sb.append("<a href=\"");

    sb.append(toUrlWithParams());
    sb.append("\" ");
    if (target != null)
      sb.append(" target=\"").append(target).append("\"");
    sb.append(">");
    sb.append(label);
    sb.append("</a>");
    return sb.toString();
  }

  public String toUrlWithParams() {
    StringBuffer sb = new StringBuffer();
    sb.append(pageSeed.getHref());
    if (!inhibitParams)
      sb.append('?' + getCEHref());
    return sb.toString();
  }

  private String getWindowFeatures() {

    if (target == null)
      return "";
    StringBuffer sb = new StringBuffer();
    sb.append(popup_resizable != null ? "resizable=" + popup_resizable + ',' : "");
    sb.append(popup_scrollbars != null ? "scrollbars=" + popup_scrollbars + ',' : "");
    sb.append(popup_toolbar != null ? "toolbar=" + popup_toolbar + ',' : "");
    sb.append(popup_height != null ? "height=" + popup_height + ',' : "");
    sb.append(popup_width != null ? "width=" + popup_width + ',' : "");

    return sb.substring(0, Math.max(sb.length() - 1, 0));
  }

  public String getLabel() {
    return label;
  }


  public static ButtonLink getTextualInstance(String label, PageSeed ps) {
    ButtonLink bl = new ButtonLink(label, ps);
    bl.outputModality = ButtonSupport.TEXT_ONLY;
    return bl;
  }

  public static ButtonLink getDescriptiveLinkInstance(String label, String href) {
    ButtonLink bl = new ButtonLink(label, new PageSeed(href));
    bl.outputModality = ButtonSupport.TEXT_ONLY;
    bl.inhibitParams=true;
    bl.target= "_blank";
    return bl;
  }

  public static ButtonLink getPopupTextualInstance(String label, int popup_height, int popup_width, PageSeed ps) {
    ButtonLink bl = getPopupInstance(label,popup_height,popup_width,ps);
    bl.outputModality = ButtonSupport.TEXT_ONLY;
    return bl;
  }

   public static ButtonLink getPopupInstance(String label, int popup_height, int popup_width, PageSeed ps) {
    ps.setPopup(true);
    ButtonLink bl = new ButtonLink(label, ps);
    bl.target = "_blank";
    bl.popup_height = popup_height+"";
    bl.popup_width = popup_width+"";
    bl.popup_scrollbars = "yes";
    bl.popup_resizable = "yes";    
    return bl;
  }

  public static ButtonSupport getBlackInstance(String label, PageSeed ps) {
    return ButtonLink.getBlackInstance(label,ps,null);
  }

  public static ButtonSupport getBlackInstance(String label, PageSeed ps, String callback) {
    return new ButtonJS(label,"openBlackPopup('"+ps.toLinkToHref()+"',$(window).width()-100,$(window).height()-50"+(JSP.ex(callback)?","+callback:"")+");");
  }

  public static ButtonSupport getBlackInstance(String label, int height, int width, PageSeed ps) {
    return getBlackInstance(label,height,width,ps,null);
  }

  public static ButtonSupport getBlackInstance(String label, int height, int width, PageSeed ps, String callback) {
    return new ButtonJS(label,"openBlackPopup('"+ps.toLinkToHref()+"','"+width+"px','"+height+"px'"+(JSP.ex(callback)?","+callback:"")+");");
  }


  public static ButtonLink getEditInstance(String editPage, IdentifiableSupport editando, HttpServletRequest request) {
    PageState pageState = PageState.getCurrentPageState(request);
    PageSeed edit = pageState.pageInThisFolder(editPage,request);
    edit.mainObjectId=editando.getId();
    edit.setCommand(Commands.EDIT);
    ButtonLink editLink = ButtonLink.getTextualInstance(I18n.get("EDIT"), edit);
    return editLink;
  }

  public static ButtonLink getEditInstanceForList(String editPage, IdentifiableSupport editando, HttpServletRequest request) {
    PageState pageState = PageState.getCurrentPageState(request);
    PageSeed edit = pageState.pageInThisFolder(editPage,request);
    edit.mainObjectId=editando.getId();
    edit.setCommand(Commands.EDIT);
    ButtonLink editLink = ButtonLink.getTextualInstance("", edit);
    editLink.iconChar="e";
    return editLink;
  }

  public static ButtonLink getDeleteInstanceForList(String editPage, IdentifiableSupport editando, HttpServletRequest request) {
    PageState pageState = PageState.getCurrentPageState(request);
    PageSeed edit = pageState.pageInThisFolder(editPage,request);
    edit.mainObjectId=editando.getId();
    edit.setCommand(Commands.DELETE_PREVIEW);
    ButtonLink editLink = ButtonLink.getTextualInstance("", edit);
    editLink.iconChar="d";
    editLink.additionalCssClass="delete";
    editLink.toolTip=getToolTipForIdentifiable(editando,pageState);
    return editLink;
  }


  public static String getToolTipForIdentifiable(IdentifiableSupport it, PageState pageState) {
    String separator="&nbsp;";

    StringBuffer result = new StringBuffer();
    if (it != null) {

      result.append("Database id: " + separator + it.getId()+ separator);
      if (it instanceof SecurableWithArea) {
        Area area = ((SecurableWithArea) it).getArea();
        result.append(separator+separator+pageState.getI18n("AREA") + separator);
        String name = null;
        if (area!=null)
           name = JSP.htmlEncodeApexesAndTags(area.getName());
        result.append(JSP.w(area != null ? name : pageState.getI18n("NO_AREA") ));
      }

      if (it instanceof LoggableIdentifiableSupport) {
        LoggableIdentifiableSupport lit = (LoggableIdentifiableSupport) it;
        String creator = JSP.htmlEncodeApexesAndTags(lit.getCreator());
        String lastModifier = JSP.htmlEncodeApexesAndTags(lit.getLastModifier());

      result.append("<br>"+pageState.getI18n("CREATED_BY") + separator + (creator != null ? creator : "-") + separator);
        if (lit.getCreationDate() != null)
          result.append(pageState.getI18n("ON_DATE") + separator + DateUtilities.dateAndHourToString(lit.getCreationDate()));
      result.append("<br>"+pageState.getI18n("LAST_MODIFIED_BY") + separator + (lastModifier != null ? lastModifier+ separator : "-" + separator));
        result.append(pageState.getI18n("ON_DATE") + separator + DateUtilities.dateAndHourToString(lit.getLastModified()) + separator);

    }
    }
    return result.toString();
  }

  public static ButtonLink getPDFFreezeButton(PageSeed pageToPrintNoContextPath, PageSeed pageToRedirToCompletePath, String fileNamePrefix) {
      PageSeed seed = new PageSeed(ApplicationState.contextPath + "/commons/tools/freezePDF.jsp");
      seed.addClientEntry("PRINTING_PDF", Fields.TRUE);
      seed.addClientEntry("PAGE_TO_PRINT",pageToPrintNoContextPath.href);
      seed.addClientEntries(pageToPrintNoContextPath.getClientEntries());
      seed.addClientEntry("REDIR_URL",pageToRedirToCompletePath.toLinkToHref());
      seed.addClientEntry("FREEZE_PREFIX",fileNamePrefix);
      seed.command=pageToPrintNoContextPath.command;
      seed.mainObjectId=pageToPrintNoContextPath.mainObjectId;
      return new ButtonLink(seed);
    }



}

