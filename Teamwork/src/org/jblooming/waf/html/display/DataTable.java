package org.jblooming.waf.html.display;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.oql.QueryHelper;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.html.button.ButtonJS;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.button.ButtonSupport;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.core.JspIncluder;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import javax.servlet.ServletException;
import javax.servlet.jsp.PageContext;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */

public class DataTable extends JspHelper implements HtmlBootstrap {
  public Form form;
  public boolean bindReturnKeyOnForm=true;

  public List headers = new ArrayList();
  public JspHelper rowDrawer;

  public String tableClass = "table";
  public String tableAdditionalAttributes = "";
  public Class<? extends ActionController> controllerClass;
  public String headerId;
  public boolean fixHead=true;
  public boolean fixFoot=true;


  public boolean drawPageFooter=false;

  public DataTable(String id, Form form, JspHelper rowsDrawer, Class<? extends ActionController> controllerClass, PageState pageState) {
    super();
    this.id = JSP.ex(id) ? id : StringUtilities.generatePassword(5); // se non lo passi lo invento
    this.form = form;
    urlToInclude = "/commons/layout/dataTable/partDataTable.jsp";
    this.headerId=id+"_HD";
    this.controllerClass = controllerClass;
    this.rowDrawer =rowsDrawer;
    pageState.sessionState.setAttribute(id, this);
  }


  public String getDiscriminator() {
    return DataTable.class.getName();
  }

  public boolean validate(PageState ps) throws IOException, ServletException {
    return ps.initedElements.contains(getDiscriminator());
  }

  /**
   * @deprecated
   */
  public void toHtml(PageContext pageContext) {
    throw new PlatformRuntimeException("Call something else");
  }

  public void drawTable(PageContext pageContext) {
    pageContext.getRequest().setAttribute(ACTION, "DRAW_TABLE");
    super.toHtml(pageContext);
  }

  public ButtonJS getSearchButton() {
    ButtonJS  sb= new ButtonJS("FLD_SEARCH", "dataTableSearchClick('"+this.id+"');"); //si svuota il filtro precotto per resettare il nome della pagina
    sb.additionalScript=" data-dtid=\""+this.id+"\"";
    sb.label = I18n.get("SEARCH");
    return sb;
  }

  public static   ButtonSupport getQBEHelpButton(PageState pageState) {
    ButtonSupport qbe = ButtonLink.getBlackInstance("", 700, 800, pageState.pageFromCommonsRoot("help/qbe.jsp"));
    //qbe.iconChar = "?";
    qbe.toolTip = I18n.get("HELP_QBE");
    qbe.label = I18n.get("HELP");
    return qbe;
  }


  //-----------------------------------------  PAGINATOR METHODS  ------------------------------------------------------------
  public void drawPaginator(PageContext pageContext) {
    pageContext.getRequest().setAttribute(ACTION, "DRAW_TBL_PAGINATOR");
    super.toHtml(pageContext);
  }

  public void drawPaginatorPagesOnly(PageContext pageContext) {
    pageContext.getRequest().setAttribute(ACTION, "DRAW_TBL_PAGINATOR");
    this.parameters.put("COMMAND","HIDETOOL");
    super.toHtml(pageContext);
  }

  public void drawPaginatorToolOnly(PageContext pageContext) {
    pageContext.getRequest().setAttribute(ACTION, "DRAW_TBL_PAGINATOR");
    this.parameters.put("COMMAND","HIDEPAGES");
    super.toHtml(pageContext);
  }


  //-----------------------------------------  HEADER METHODS  ------------------------------------------------------------
  public void drawTableHeaders(PageContext pageContext) {
    pageContext.getRequest().setAttribute(ACTION, "DRAW_TBL_HEADERS");
    super.toHtml(pageContext);
  }


  public void addHeader(String label) {
    addHeader(label, null, null);
  }

  public void addHeaderFitAndCentered(String label) {
    addHeader(label, "1%", null);
  }

  public void addHeader(String label, String orderingHql) {
    addHeader(label, null, orderingHql);
  }

  public void addHeaderFitAndCentered(String label, String orderingHql) {
    addHeader(label, "1%", orderingHql);
  }

  public void addHeader(String label, String width, String orderingHql) {
    addHeader(label, width, null, orderingHql);
  }

  public void addHeader(String label, String width, String align, String orderingHql) {
    addHeader(label,width,align,orderingHql,null);
  }
  public void addHeader(String label, String width, String align, String orderingHql, String id) {
    final TableHeaderElement header = new TableHeaderElement();
    header.id=id;
    header.orderingHql = orderingHql;
    header.width = width;
    header.align = align;
    header.label=label;
    headers.add(header);
  }

  public void addHeader(JspIncluder jspIncluder) {
    headers.add(jspIncluder);
  }

  public List getHeaders() {
    return headers;
  }

  public class TableHeaderElement {
    public String id;
    public String orderingHql;
    public String width;
    public String align;
    public String label;
    public int state = 0;
  }



  //-----------------------------------------  TABLE ROWS METHODS  ------------------------------------------------------------
  public void drawTableRows(PageContext pageContext) {
    pageContext.getRequest().setAttribute(ACTION, "DRAW_TBL_ROWS");
    super.toHtml(pageContext);
  }

  //chiama il rowDrawer con una ACTION=DRAW_TBL_FOOT
  public void drawTableFooter(PageContext pageContext) {
    pageContext.getRequest().setAttribute(ACTION, "DRAW_TBL_FOOT");
    rowDrawer.toHtml(pageContext);
  }





  public static void orderAction(QueryHelper qhelp, String s, RestState pageState) {
    orderAction(qhelp, s, pageState, null);
  }

  /**
   * must be called by actions of list of HibernatePages
   *
   * @param qhelp
   * @param listHeaderId
   * @param pageState
   */
  public static void orderAction(QueryHelper qhelp, String listHeaderId, RestState pageState, String defaultOrder) {

    String orderBy = pageState.getEntry(Form.FLD_FORM_ORDER_BY + listHeaderId).stringValueNullIfEmpty();
    if (orderBy==null) {
      orderBy = defaultOrder;
      pageState.addClientEntry(Form.FLD_FORM_ORDER_BY + listHeaderId,defaultOrder);
    }

    if (JSP.ex(orderBy)){
      int pos = qhelp.getHqlString().toLowerCase().indexOf(" order by ");
      if (pos >-1 ){
        String hql=qhelp.getHqlString().substring(0,pos);
        qhelp.setHqlString(hql);
      }
      qhelp.addToHqlString(" order by " + orderBy);
    }

  }

}
