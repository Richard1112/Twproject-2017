package org.jblooming.waf.html.display;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.operator.Operator;
import org.jblooming.operator.businessLogic.OptionAction;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import javax.servlet.ServletException;
import javax.servlet.jsp.PageContext;
import java.io.IOException;
import java.text.ParseException;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class Paginator extends JspHelper implements HtmlBootstrap {
  public static final String init = Paginator.class.getName();
  public Form form;
  public static final String FLD_PAGE_NUMBER = "_FP_PG_N";
  public static final String FLD_PAGE_SIZE = "_FP_PG_S";

  public static final int DEFAULT_PAGE_SIZE = 10;


  public Paginator(String id, Form form) {
    super();
    this.id = id;
    this.form = form;
    urlToInclude = "/commons/layout/partPagePaginator.jsp";
  }

  public String getDiscriminator() {
    return init+id;
  }

  public boolean validate(PageState ps) throws IOException, ServletException {
    return ps.initedElements.contains(init);
  }

  public static int getWantedPageNumber(RestState pageState) {
    int result = 0;
    try {
      result = Math.max(0, pageState.getEntry(FLD_PAGE_NUMBER).intValue() - 1);

    } catch (ActionException e) {
    } catch (ParseException e) {
    }
    return result;
  }

  /**
   * Default page size = 6
   *
   * @param pageState
   */
  public static int getWantedPageSize(RestState pageState) {
    return getWantedPageSize("",0,pageState);
  }

  public static int getWantedPageSize(String pageName, RestState pageState) {
    return getWantedPageSize(pageName, 0, pageState);
  }

  public static int getWantedPageSize(String pageName, int defaultSize, RestState pageState) {

    if (defaultSize == 0) {
      defaultSize = DEFAULT_PAGE_SIZE;
      String option = Operator.getOperatorOption(pageState.getLoggedOperator(), OperatorConstants.OP_PAGE_SIZE + pageName);
      if (option != null && option.trim().length() > 0)
        defaultSize = Integer.parseInt(option);
      else {
        option = Operator.getOperatorOption(pageState.getLoggedOperator(), OperatorConstants.OP_PAGE_SIZE);
        if (option != null && option.trim().length() > 0)
          defaultSize = Integer.parseInt(option);
      }
    }

    try{
      int currentValue = pageState.getEntry(FLD_PAGE_SIZE).intValueNoErrorCodeNoExc();
      if (currentValue !=0 && currentValue != defaultSize) {
        OptionAction.cmdUpdateLoggedOption(pageState, OperatorConstants.OP_PAGE_SIZE + pageName, currentValue + "");
      }
      if (currentValue <= 0)
        currentValue = defaultSize;
      pageState.addClientEntry(FLD_PAGE_SIZE,currentValue);
      return currentValue;
    } catch (PersistenceException e) {
      throw new PlatformRuntimeException(e);
    }
  }


  public void toHtml(PageContext pageContext) {
    init(pageContext);
    pageContext.getRequest().removeAttribute(ACTION );
    super.toHtml(pageContext);
  }

  public void reDrawPaginator(PageContext pageContext) {
    init(pageContext);
    this.parameters.put("HIDETOOL","stica");
    pageContext.getRequest().removeAttribute(ACTION );
    super.toHtml(pageContext);
    this.parameters.remove("HIDETOOL");
  }


  public void init(PageContext pageContext) {
    PageState ps = PageState.getCurrentPageState(pageContext);
    if (!ps.initedElements.contains(getDiscriminator())) {
      pageContext.getRequest().setAttribute(ACTION, INITIALIZE);
      super.toHtml(pageContext);
      ps.initedElements.add(getDiscriminator());
    }
  }


}
