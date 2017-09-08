package com.opnlb.website.news.businessLogic;

import com.opnlb.website.news.News;
import com.opnlb.website.security.WebSitePermissions;
import org.jblooming.ApplicationException;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.HttpUtilities;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.FieldErrorConstants;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.display.DataTable;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.html.input.Uploader;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.state.PersistentSearch;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import java.io.File;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * NewsAction (c) 2005 - Open Lab - www.open-lab.com
 */
public class NewsAction {

  public void cmdAdd(PageState pageState) {
    News mainObject = new News();
    mainObject.setIdAsNew();
    pageState.setMainObject(mainObject);
    make(mainObject, pageState);
  }

  public void cmdEdit(PageState pageState) throws PersistenceException {
    News news = (News) PersistenceHome.findByPrimaryKey(News.class, pageState.getMainObjectId());
    pageState.setMainObject(news);
    make(news, pageState);
  }

  public void cmdFind(PageState pageState) throws PersistenceException, ActionException {
    Operator op = pageState.getLoggedOperator();
    //defaults
    boolean somethingSearched = false;
    String filter = null;
    String hql = "select distinct news from " + News.class.getName() + " as news ";

    if (pageState.getEntry(Form.FLD_FORM_ORDER_BY + "NEWSMGR").stringValueNullIfEmpty() == null) {
      hql = hql + " order by news.orderFactor, news.lastModified desc, news.endingDate desc, news.title ";
    }
    QueryHelper qhelp = new QueryHelper(hql);

    boolean recoveredFromSavedFilter = PersistentSearch.feedFromSavedSearch(pageState);

    filter = pageState.getEntry("TITLES_TEXT").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQueryClause(qhelp.getQbeClause("news.title", "title", filter, QueryHelper.TYPE_CHAR)
        + " OR " +
        qhelp.getQbeClause("news.subTitle", "subTitle", filter, QueryHelper.TYPE_CHAR)
        + " OR " +
        qhelp.getQbeClause("news.text", "text", filter, QueryHelper.TYPE_CHAR));
      somethingSearched = true;
    }

    filter = pageState.getEntry("START").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEClause("news.startingDate", "startingDate", filter, QueryHelper.TYPE_DATE);
      somethingSearched = true;
    }

    filter = pageState.getEntry("END").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEClause("news.endingDate", "endingDate", filter, QueryHelper.TYPE_DATE);
      somethingSearched = true;
    }

    filter = pageState.getEntry("VISIBLE").stringValueNullIfEmpty();
    if (filter != null && !"ALL".equals(filter)) {
      if (Fields.TRUE.equals(filter))
        qhelp.addOQLClause("news.visible=:visible", "visible", Boolean.TRUE);
      else
        qhelp.addOQLClause("news.visible=:visible", "visible", Boolean.FALSE);

      somethingSearched = true;
    }

    if (!somethingSearched && Commands.FIND.equals(pageState.getCommand())) {
      qhelp.addQBEClause("news.title", "news", "*", QueryHelper.TYPE_CHAR);
    }

    DataTable.orderAction(qhelp, "NEWSMGR", pageState);
    pageState.setPage(HibernatePage.getHibernatePageInstance(qhelp.toHql().getQuery(), Paginator.getWantedPageNumber(pageState), Paginator.getWantedPageSize(pageState)));
  }

  public void cmdSave(HttpServletRequest request, PageState pageState) throws PersistenceException, ActionException, ApplicationException {

    News news = null;
    boolean invalidClientEntries = false;

    if (PersistenceHome.NEW_EMPTY_ID.equals(pageState.getMainObjectId())) {
      news = new News();
      news.setIdAsNew();
    } else {
      news = (News) PersistenceHome.findByPrimaryKey(News.class, pageState.getMainObjectId());
    }
    pageState.setMainObject(news);

    //validity check start
    String title = pageState.getEntryAndSetRequired("TITLE").stringValueNullIfEmpty();

    Date start = pageState.getEntry("START").dateValueNoErrorNoCatchedExc();
    Date end = pageState.getEntry("END").dateValueNoErrorNoCatchedExc();
    if (end != null && start != null && end.getTime() < start.getTime()) {
      invalidClientEntries = true;
      pageState.getEntry("END").errorCode = pageState.getI18n("END_MUST_BE_AFTER_START");
    }
    //validity check end

    String text = pageState.getEntry("TEXT").stringValueNullIfEmpty();
    if (text != null && text.length() > 2999) {
      invalidClientEntries = true;
      ClientEntry ceNull = new ClientEntry("TEXT", text.substring(0, 2995));
      ceNull.errorCode = pageState.getI18n("MAX_LENGTH") + ": 3000";
      pageState.addClientEntry(ceNull);
    }

    if (pageState.validEntries() && !invalidClientEntries) {
      news.setTitle(title);

      ActionUtilities.setString(pageState.getEntry("SUBTITLE"), news, "subTitle");
      ActionUtilities.setString(pageState.getEntry("TEXT"), news, "text");

      news.setVisible(pageState.getEntry("VISIBLE").checkFieldValue());
      news.setStartingDate(start);
      news.setEndingDate(end);

      news.setImageWidth(pageState.getEntry("IMG_WIDTH").intValueNoErrorCodeNoExc());
      news.setImageHeight(pageState.getEntry("IMG_HEIGHT").intValueNoErrorCodeNoExc());
      news.setOrderFactor(pageState.getEntry("ORDER_FACTOR").intValueNoErrorCodeNoExc());

      news.store();


      String ext = "";
      String uploadingFile = pageState.getEntry("IMAGE").stringValueNullIfEmpty();
      PersistentFile image = news.getImage();


      if (uploadingFile != null) {
        ext = FileUtilities.getFileExt(uploadingFile);
        if (FileUtilities.isImageByFileExt(ext) ) {
          if (image != null) {
            image.delete();
          }
          news.setImage(Uploader.save(news, new PersistentFile(0, null, PersistentFile.DEFAULT_STORAGE_TYPE), "IMAGE", pageState));

        } else {
          ClientEntry ceNull = new ClientEntry("IMAGE", null);
          ceNull.errorCode = pageState.getI18n("NOT_ALLOWED_FILE_EXT");
          pageState.addClientEntry(ceNull);
        }
      } else {
        final ClientEntry entry = pageState.getEntry("IMAGE");
        String value = pageState.getEntry("IMAGE").stringValueNullIfEmpty();
        boolean fileSelected = value != null;
        boolean alreadyPersisted = (image != null && image.getUID() != 0);
        boolean uploadEntryMissing = entry.name == null;
        boolean removeIt = !uploadEntryMissing && alreadyPersisted && !fileSelected;
        if (image != null && removeIt) {
          String fileLoc = HttpUtilities.getFileSystemRootPathForRequest(request) + image.getFileLocation();
          File delendo = new File(fileLoc);
          FileUtilities.tryHardToDeleteFile(delendo);
        }
        news.setImage(Uploader.save(news, image, "IMAGE", pageState));
      }
    }
  }

  private void make(News news, PageState pageState) {

    pageState.addClientEntry("ID", news.getId());
    pageState.addClientEntry("TITLE", news.getTitle());
    pageState.addClientEntry("SUBTITLE", news.getSubTitle());
    pageState.addClientEntry("TEXT", news.getText());
    pageState.addClientEntry("IMG_WIDTH", (news.getImageWidth() != null ? news.getImageWidth() : 0));
    pageState.addClientEntry("IMG_HEIGHT", (news.getImageHeight() != null ? news.getImageHeight() : 0));
    pageState.addClientEntry("START", news.getStartingDate() != null ? "" + DateUtilities.dateToString(news.getStartingDate()) : "");
    pageState.addClientEntry("END", news.getEndingDate() != null ? "" + DateUtilities.dateToString(news.getEndingDate()) : "");
    pageState.addClientEntry("OWNER", news.getOwner() != null ? news.getOwner().getId() : "");
    pageState.addClientEntry("VISIBLE", (news.isVisible() ? Fields.TRUE : Fields.FALSE));
    pageState.addClientEntry("ORDER_FACTOR", (news.getOrderFactor() != null ? news.getOrderFactor() : 0));

    if (news.getImage() != null) {
      pageState.addClientEntry("IMAGE", news.getImage());
    } else {
      pageState.addClientEntry("IMAGE", "");
    }
  }

  public void cmdDelete(PageState pageState, HttpServletRequest request) throws PersistenceException {
    //user.testPermission(PlatformPermissions.role_canWrite);
    News delenda = (News) PersistenceHome.findByPrimaryKey(News.class, pageState.getMainObjectId());
    String basePath = HttpUtilities.getFileSystemRootPathForRequest(request);
    PersistentFile imageFile = delenda.getImage();
    if (imageFile != null) {
      String uploadedFilePathName = basePath + delenda.getImage().getFileLocation();
      FileUtilities.tryHardToDeleteFile(new File(uploadedFilePathName));
    }
    DeleteHelper.cmdDelete(delenda, pageState);
  }

  public static int categoryPresent(Operator logged, String categoryClassName) throws FindException {
    boolean canWorkNews = (logged != null && !logged.hasPermissionAsAdmin());
    List categoryList = new ArrayList();
    // admin
    String hql = " from " + categoryClassName + " as cat order by cat.description";
    // is not admin but hasPermissionFor
    if (canWorkNews) {
      hql = " select distinct cat.id from " + categoryClassName + " as cat join cat.newsCategoryManager as managers " +
        " where managers.operator.id=:opId";
    }
    OqlQuery oqlCat = new OqlQuery(hql);

    if (canWorkNews)
      oqlCat.getQuery().setString("opId", logged.getId().toString());

    categoryList = oqlCat.list();
    return categoryList.size();
  }

  public void cmdMakeVisible(PageState pageState) throws FindByPrimaryKeyException, StoreException {
    News news = (News) PersistenceHome.findByPrimaryKey(News.class, pageState.getMainObjectId());
    news.setVisible(true);
    news.store();
  }

  public void cmdMakeInvisible(PageState pageState) throws FindByPrimaryKeyException, StoreException {
    News news = (News) PersistenceHome.findByPrimaryKey(News.class, pageState.getMainObjectId());
    news.setVisible(false);
    news.store();
  }

}
