package com.twproject.document.businessLogic;

import com.twproject.operator.TeamworkOperator;
import com.twproject.security.TeamworkPermissions;
import org.jblooming.ApplicationException;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.remoteFile.FileStorage;
import org.jblooming.security.Area;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.PlatformConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.display.DataTable;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Set;

public class FileStorageController implements ActionController {



  public PageState perform(HttpServletRequest request, HttpServletResponse response)
    throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException {

    PageState pageState = PageState.getCurrentPageState(request);

    FileStorageAction fsa = new FileStorageAction(pageState);
    String command = pageState.getCommand();

    if (Commands.ADD.equals(command) ){//|| !JSP.ex(command)) {
      fsa.cmdAdd();

    } else if (Commands.EDIT.equals(command) || Commands.DELETE_PREVIEW.equals(command))
      fsa.cmdEdit();

    else if (Commands.SAVE.equals(command))
      fsa.cmdSave();

    else if (Commands.DELETE.equals(command)) {
      try {
        fsa.cmdDelete();
        PageSeed ps = pageState.pageFromRoot("document/fileStorageList.jsp");
        //pageState.redirect(ps);

        if (JSP.ex(pageState.getEntry("DO_NOT_REDIR")))
          pageState.addClientEntry("REDIRECT_TO",ps.toLinkToHref());
        else
          pageState.redirect(ps);

      } catch (RemoveException ex) {
        // in order to feedback operator in partDelete.jsp
        pageState.setAttribute(PlatformConstants.DELETE_EXCEPTION, ex);
      }
    }  else if (Commands.FIND.equals(command)) {
        TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
        String hql = "from " + FileStorage.class.getName() + " as document";

      QueryHelper qh = new QueryHelper(hql);

      // filter for areas
      Set<Area> al = logged.getAreasForPermission(TeamworkPermissions.fileStorage_canRead);
      if (al.size() > 0)
        qh.addOQLClause("document.area in (:al)", "al", al);

      qh.addOrQueryClause("document.owner = :myself");
      qh.setParameter("myself", logged);

      DataTable.orderAction(qh, "DOCFS", pageState, "document.name");
      HibernatePage page = HibernatePage.getHibernatePageInstance(qh.toHql().getQuery(), Paginator.getWantedPageNumber(pageState), Paginator.getWantedPageSize("ISSUEFILTER", pageState));
      pageState.setPage(page);


    }


    return pageState;
  }


}
