package org.jblooming.logging.businessLogic;

import com.twproject.operator.TeamworkOperator;
import com.twproject.security.TeamworkPermissions;
import org.jblooming.ApplicationException;
import org.jblooming.logging.DeleteLog;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.SecurityException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.display.DataTable;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.html.table.ListHeader;
import org.jblooming.waf.state.PersistentSearch;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Created by rbicchierai on 08/05/2017.
 */
public class DeleteLogController implements ActionController {


  public TeamworkOperator logged;
  public String url;


  public PageState perform(HttpServletRequest request, HttpServletResponse response)
    throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException {

    PageState pageState = PageState.getCurrentPageState(request);

    String command = pageState.getCommand();

    DeleteLogAction deleteLogAction = new DeleteLogAction(pageState);
    if (Commands.FIND.equals(command)) {
    deleteLogAction.cmdFind();
  } else {
    deleteLogAction.cmdPrepareDefaultFind();
  }

    return pageState;
  }


}
