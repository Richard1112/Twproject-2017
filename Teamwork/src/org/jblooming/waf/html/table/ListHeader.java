package org.jblooming.waf.html.table;

import org.jblooming.utilities.JSP;
import org.jblooming.oql.*;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.button.ButtonSubmit;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import javax.servlet.jsp.PageContext;
import java.util.ArrayList;
import java.util.List;


public class ListHeader extends JspHelper implements HtmlBootstrap {
  private List headers = new ArrayList();
  public Form form;

  // used to ajax submit style
  private boolean ajaxEnabledLoc =false;
  private String  ajaxDomIdToReloadLoc =null;



  public ListHeader(String id, Form form) {
    this.id = id;
    urlToInclude = "/commons/layout/partListHeader.jsp";
    this.form = form;
  }


  public static ListHeader getAjaxInstance(String id, Form form, String domIdToRelead){
    ListHeader ret= new ListHeader(id,form);
    ret.ajaxEnabledLoc = true;
    ret.ajaxDomIdToReloadLoc=domIdToRelead;
    return ret;
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
    ListHeaderButton bs = new ListHeaderButton(form);
    final Header header = new Header();
    header.bs = bs;
    header.orderingHql = orderingHql;
    bs.drawOrderBy = orderingHql != null;
    header.width = width;
    header.align = align;
    header.setLabel(label);
    getHeaders().add(header);
  }

  public String getDiscriminator() {
    return ListHeader.class.getName();
  }

  public boolean validate(PageState pageState) {
    return true;
  }

  public List getHeaders() {
    return headers;
  }

  public class Header {
    public ListHeaderButton bs;
    public String orderingHql;
    public String width;
    public String align;
    private String label;
    public int state = UNORDERED;
    public static final int UNORDERED = 0;
    public static final int ASCENDING = 1;
    public static final int DESCENDING = 2;

    public void toHtml(PageContext pageContext)  {
      PageState pageState = PageState.getCurrentPageState(pageContext);
      if (bs.drawOrderBy) {
        try {
          String orderBy = pageState.getEntry(Form.FLD_FORM_ORDER_BY + id).stringValue();
          if (orderingHql != null && orderBy!=null &&  orderBy.indexOf("asc") > -1 && orderBy.indexOf(orderingHql.trim()) > -1)
            state = ASCENDING;
          else if (orderingHql != null && orderBy!=null && orderBy.indexOf("desc") > -1 && orderBy.indexOf(orderingHql.trim()) > -1)
            state = DESCENDING;
        } catch (ActionException e1) {
          state = UNORDERED;
        }

        if (state == UNORDERED) {
          bs.label = getLabel() + "&nbsp;<span class='teamworkIcon'>&para;</span>";
          bs.variationsFromForm.addClientEntry(Form.FLD_FORM_ORDER_BY + id, orderingHql + " asc");
          bs.toolTip = I18n.get("TO_ASC_TITLE");
        } else if (state == ASCENDING) {
          bs.label = getLabel() + "&nbsp;<span class='teamworkIcon'>u</span>";

          bs.toolTip = I18n.get("ASC_TO_DESC_TITLE");
          bs.variationsFromForm.addClientEntry(Form.FLD_FORM_ORDER_BY + id, orderingHql + " desc");
        } else {
          bs.label = getLabel() + "&nbsp;<span class='teamworkIcon'>&ugrave;</span>";
          bs.toolTip = I18n.get("DESC_TO_UNORD_TITLE");
          bs.variationsFromForm.addClientEntry(Form.FLD_FORM_ORDER_BY + id, "");
        }
      } else {
        bs.label = getLabel();
        bs.toolTip = getLabel();
      }
      bs.alertOnChange = false;
      bs.toHtml(pageContext);
    }

    public String getLabel() {
      return label;
    }

    public void setLabel(String label) {
      this.label = label;
    }
  }

  public class ListHeaderButton extends ButtonSubmit {
    public boolean drawOrderBy = true;

    public ListHeaderButton(Form form) {
      super(form);
      this.ajaxDomIdToReload= ajaxDomIdToReloadLoc;
      this.ajaxEnabled=ajaxEnabledLoc;
      urlToInclude = "/commons/layout/partListHeaderButton.jsp";
    }
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
