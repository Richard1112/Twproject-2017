package org.jblooming.waf.html.input;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageState;

import javax.servlet.ServletException;
import javax.servlet.jsp.PageContext;
import java.io.IOException;
import java.sql.*;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * The smart combo is a composite object. It is made by the hidden field (called "fieldName") were the selected ID is stored.
 * Usually you should refer this field. In some cases you'll like refer to the value of inserted input text (for instance if you want to
 * create filter businessLogic "on the fly") that is called  "fieldName"+TEXT_FIELD_POSTFIX. The third component is the iframe, shared between
 * all the combo's in the page.
 * SmartCombo uses onBlur, onKeyUp, onFocus javascript event. If you need to add handle you must use the onBlurAdditionalScript property.
 * <p/>
 * toHtml saves in a map on session with key fieldName and value itself.
 * this in order to make iself instantiatible from the part of jsp that produces the list;
 * this is limited in the sense that if two pages in the same session simultaneously instantiate a combo
 * with the same field name, there might be leaked of filtered data among them. it may be improved on need.
 */
public class SQLCombo extends JspHelper implements HtmlBootstrap {

  public boolean shooted = false;

  public boolean convertToUpper = false;

  public String fieldName;
  public String fieldClass = "formElements smartCombo";
  public String dropDownFieldClass = null;
  public String separator = "&nbsp;";
  public String label;
  public String innerLabel; // printed inside the field

  public String initialSelectedCode;
  public int tabIndex;
  public int fieldSize;
  public String script;
  public String onBlurAdditionalScript;
  public boolean disabled;
  public boolean firstEmpty = false; //used in classic moodality only

  public int columnToCopyInDescription = 1;


  /**
   * The query must select at least two properties (in case you have one, just repeat it) limited to the select part.
   * Then there are two mandatory parameter "whereForFiltering" and "whereForId" to specify how to retrieve data
   * Both MUST uses the parameter named SmartCombo.FILTER_PARAM_NAME. Additional fixed parameter filter must be defined
   * in the query and set on "fixedParams" map
   * <p/>
   * Of the projected properties (columns for the not so priviledged):
   * 0 element is used as value for the hidden field (ID usually)
   * columnToCopyInDescription element is used for filterning and hence to fill the text field
   * from 1 onwards are used for display
   */
  public String sqlSelect;
  public String whereForFiltering;
  public String whereForId;

  public String jdbcDriver;
  public String dbConnectionString;
  public String dbUser;
  public String dbPassword;
  public boolean useDefaultConnection = true;

  /**
   * this query is used when a master-slave smart combos are in action. This query is similar to "whereForFiltering" but must contains a #PARAM# in the string.
   * #PARAM# will be replaced by ajax when master changes
   */
  public String whereForRefresh;


  /**
   * key control
   */
  public String actionListened;
  public int keyToHandle;
  public String launchedJsOnActionListened;
  public static final String TEXT_FIELD_POSTFIX = "_txt";
  public static final String INITIALIZE = "IN";
  public static final String DRAW_INPUT = "DI";

  public int iframe_width = 300;
  public int iframe_height = 100;

  public int maxRowToFetch = 20;
  /**
   * teoros
   * if false nothing changes, search for filter+"%"
   * if true search for "%"+filter+"%"
   */
  public boolean searchAll = false;

  /**
   * a valid value must be selected
   */
  public boolean required = false;
  public boolean readOnly = false;


  public boolean preserveOldValue = true;

  /**
   * this script if filled is launched when a value for the smart combo has been selected. In the function context "this" is the hidden compo input field
   */
  public String onValueSelectedScript;

  /**
   * this set of values are always added in top of others and respective objects are removed from the query part.
   * The format MUST be congruent with the query passed
   */
  public List<Object[]> additionalLines = null;
  public Set<Object> highlightedIds = new HashSet();


  public SQLCombo() {
    this.urlToInclude = "/commons/layout/smartCombo/partSQLCombo.jsp";
    this.maxRowToFetch= Integer.parseInt(ApplicationState.getApplicationSetting("COMBO_ROWS_TO_FETCH", "20"));
  }

  public SQLCombo(String fieldName, String select, String whereForFiltering, String whereForId) {
    this();
    this.fieldName = fieldName;
    this.sqlSelect = select;
    this.whereForFiltering = whereForFiltering;
    this.whereForId = whereForId;
  }

  public static SQLCombo getSimpleSQLCombo(String fieldName, String tableName, String idColumnName, String descriptionColumnName) {
    SQLCombo sqlCombo = new SQLCombo(fieldName,
      "select " + idColumnName + ", " + descriptionColumnName + " from " + tableName,
      "where upper(" + descriptionColumnName + ") like ? order by " + descriptionColumnName,
      "where " + idColumnName + " = ?");
    sqlCombo.convertToUpper=true;
    return sqlCombo;
  }

  public void setConnectionData(String driver, String url, String user, String password) {
    this.jdbcDriver = driver;
    this.dbConnectionString = url;
    this.dbUser = user;
    this.dbPassword = password;
    this.useDefaultConnection = false;
  }


  public String getDiscriminator() {
    return SQLCombo.class.getName();
  }


  public boolean validate(PageState pageState) throws IOException, ServletException {
    return true;
  }

  public void toHtml(PageContext pageContext) {
    if (shooted)
      throw new RuntimeException("You cannot call toHtml twice in case of smartCombo");
    shooted = true;

    pageContext.getRequest().setAttribute(ACTION, DRAW_INPUT);
    super.toHtml(pageContext);
  }

  public void toHtmlI18n(PageContext pageContext) {
    PageState pageState = PageState.getCurrentPageState(pageContext);
    if (label == null)
      label = pageState.getI18n(fieldName);
    else
      label = pageState.getI18n(label);
    toHtml(pageContext);
  }

  public static String getHiddenFieldName(String ceName) {
    return ceName + TEXT_FIELD_POSTFIX;
  }


  public List<Object[]> fillResultList(String filter, String hiddenValue) {
    PreparedStatement statement = null;
    List<Object[]> prs = new ArrayList();
    Connection connection = null;
    try {

      if (JSP.ex(this.dbConnectionString) && JSP.ex(this.dbUser) && JSP.ex(this.jdbcDriver)) {
        Class.forName(jdbcDriver);
        connection = DriverManager.getConnection(dbConnectionString, dbUser, dbPassword);
        useDefaultConnection = false;
      } else {
        connection = PersistenceContext.getDefaultPersistenceContext().session.connection();
      }


      // if there is any hidden value fill the first row
      if (JSP.ex(hiddenValue) && !id.matches("(##[^#]*##)|(\\(#[^#]*#\\))")) {


        statement = connection.prepareStatement(sqlSelect + " " + whereForId);
        statement.setMaxRows(1);

        //ci possono essere problemi con il tipo di dati int o String
        int columnType = statement.getMetaData().getColumnType(1);
        if (columnType == Types.INTEGER || columnType == Types.NUMERIC || columnType == Types.BIGINT) {
          int intId = 0;
          try {
            intId = Integer.parseInt(hiddenValue);
          } catch (NumberFormatException nfe) {
            Tracer.platformLogger.error("Invalid id type id:" + hiddenValue);
          }
          statement.setObject(1, intId);
        } else
          statement.setString(1, hiddenValue);

        ResultSet resultSet = statement.executeQuery();
        int numOfCol = statement.getMetaData().getColumnCount();
        if (resultSet.next()) {
          Object[] oa = new Object[numOfCol];

          for (int i = 1; i <= numOfCol; i++) {
            oa[i - 1] = resultSet.getObject(i);
          }
          prs.add(oa);
        }
        resultSet.close();
        statement.close();
      }

      // add additional lines if any
      if ((JSP.ex(hiddenValue) || !JSP.ex(filter)) && JSP.ex(additionalLines)) {
        //is the first line is in the additional do not add it again
        for (Object[] addLine : additionalLines) {
          highlightedIds.add(addLine[0]);
          if (prs.size() == 0 || !addLine[0].equals(prs.get(0)[0])) {
            prs.add(addLine);
          }
        }
      }

      //copy the elements for check duplicated
      List<Object[]> alreadyThere = new ArrayList(prs);

      //if is the first search and there is few lines remove the filter in order to fill with a bunch of lines
      if (JSP.ex(hiddenValue) && prs.size() < 4) {
        filter = "";
      }


      //si converte in upper
      if (convertToUpper)
        filter=filter.toUpperCase();

      statement = connection.prepareStatement(sqlSelect + " " + whereForFiltering);
      statement.setString(1, (searchAll ? "%" + filter + "%" : filter + "%"));
      statement.setMaxRows(maxRowToFetch);
      ResultSet resultSet = statement.executeQuery();
      int numOfCol = statement.getMetaData().getColumnCount();

      // fill the result list if the line in not already there
      while (resultSet.next()) {
        boolean found = false;
        for (Object[] existLine : alreadyThere) {
          if (resultSet.getObject(1).equals(existLine[0])) {
            found = true;
            break;
          }
        }
        if (!found) {
          Object[] oa = new Object[numOfCol];
          for (int i = 1; i <= numOfCol; i++) {
            oa[i - 1] = resultSet.getObject(i);
          }
          prs.add(oa);
        }
      }
      resultSet.close();
      statement.close();

    } catch (Throwable t) {
      Tracer.platformLogger.error(t);
      throw new PlatformRuntimeException(t);
    } finally {
      try {
        if (!useDefaultConnection && connection != null) {
          connection.close();
        }
      } catch (Throwable t) {
        throw new PlatformRuntimeException(t);
      }
    }
    return prs;
  }


  public String getTextValue(PageState pageState) {
    String id = pageState.getEntry(fieldName).stringValueNullIfEmpty();
    return getTextValue(id);
  }

  public String getTextValue(String id) {
    String ret = "";

    if (JSP.ex(id) && !id.matches("(##[^#]*##)|(\\(#[^#]*#\\))")) { // avoid call when JST template is in action

      Object[] result = null;
      Connection connection = null;
      try {

        if (JSP.ex(this.dbConnectionString) && JSP.ex(this.dbUser) && JSP.ex(this.jdbcDriver)) {
          Class.forName(jdbcDriver);
          connection = DriverManager.getConnection(dbConnectionString, dbUser, dbPassword);
          useDefaultConnection = false;
        } else {
          connection = PersistenceContext.getDefaultPersistenceContext().session.connection();
        }


        PreparedStatement statement = connection.prepareStatement(sqlSelect + " " + whereForId);

        //ci possono essere problemi con il tipo di dati int o String
        int columnType = statement.getMetaData().getColumnType(1);
        if (columnType == Types.INTEGER || columnType == Types.NUMERIC || columnType == Types.BIGINT)
          statement.setObject(1, Integer.parseInt(id));
        else
          statement.setString(1, id);

        ResultSet resultSet = statement.executeQuery();
        int numOfCol = statement.getMetaData().getColumnCount();
        if (resultSet.next()) {
          result = new Object[numOfCol];

          for (int i = 1; i <= numOfCol; i++) {
            result[i - 1] = resultSet.getObject(i);
          }
        }


      } catch (Throwable e) {
        //this can happen when SQLCombo is used in JST teplates when id -> ##id## or (#obj.id#)
        // Tracer.platformLogger.error(e);
      } finally {
        try {
          if (!useDefaultConnection && connection != null) {
            connection.close();
          }
        } catch (Throwable t) {
          throw new PlatformRuntimeException(t);
        }

      }

      if (result != null) {
        ret = result[columnToCopyInDescription] + "";
      }
    }


    return ret;
  }

}
