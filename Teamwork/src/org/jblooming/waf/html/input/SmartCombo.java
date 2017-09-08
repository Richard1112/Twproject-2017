package org.jblooming.waf.html.input;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletException;
import javax.servlet.jsp.PageContext;

import org.hibernate.HibernateException;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.ontology.Lookup;
import org.jblooming.ontology.LookupInt;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.button.ButtonSupport;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.PersistenceConfiguration;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

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
public class SmartCombo extends JspHelper implements HtmlBootstrap {

  public boolean shooted = false; //usato se si vuole evitare che venga inizializzato

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
  public int maxLenght;
  public String script;
  public String onBlurAdditionalScript;
  public boolean disabled;
  public boolean firstEmpty = false; //used in classic moodality only

  public ButtonSupport linkToEntity;
  public ButtonSupport addEntityButton=null; //bottone aggiunto in fondo al drop down per andare alla creazione di elementi

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
  public String hql;
  public String whereForFiltering;
  public String whereForId;

  /**
   * this query is used when a master-slave smart combos are in action. This query is similar to "whereForFiltering" but must contains a #PARAM# in the string.
   * #PARAM# will be replaced by ajax when master changes
   */
  public String whereForRefresh;

  /**
   * these usage is left as an exercise
   */
  public QueryHelper queryHelperForFiltering;

  /**
   * additional fixed values for the sqlSelect query. Used to set fixed filter (eg.: "hidden" , true)
   */
  public Map<String, Object> fixedParams = new HashTable();

  /**
   * key control
   */
  public String actionListened;
  public int keyToHandle;
  public String launchedJsOnActionListened;
  public static final String TEXT_FIELD_POSTFIX = "_txt";
  public static final String LINKENTITY_POSTFIX = "_lnk";
  public static final String INITIALIZE = "IN";
  public static final String DRAW_INPUT = "DI";
  public static final String FILTER_PARAM_NAME = "filter";

  public int iframe_width = 400;
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

  public boolean classic = false;

  /**
   * if addAllowed = true no error for un-existing values. Should be used if new values are added in the controller
   */
  public boolean addAllowed = false;

  public boolean preserveOldValue = true;

  /**
   * this script if filled is launched when a value for the smart combo has been selected. In the function context "this" is the hidden input field
   */
  public String onValueSelectedScript;

  /**
   * this set of values are always added in top of others and respective objects are removed from the query part.
   * The format MUST be congruent with the query passed
   */
  public List<Object[]> additionalLines = null;
  public Set<Object> highlightedIds = new HashSet();


  public SmartCombo() {
    this.urlToInclude = "/commons/layout/smartCombo/partSmartCombo.jsp";
    this.maxRowToFetch= Integer.parseInt(ApplicationState.getApplicationSetting("COMBO_ROWS_TO_FETCH", "20"));
  }

  public SmartCombo(String fieldName, String hql, String whereForFiltering, String whereForId) {
    this();
    this.fieldName = fieldName;
    this.hql = hql;
    this.whereForFiltering = whereForFiltering;
    this.whereForId = whereForId;
  }

  public SmartCombo(String fieldName, Class<? extends Lookup> lookupClass) {
    this(fieldName,
            "select p.id, p.description from " + lookupClass.getName() + " as p",
            "where upper(p.description) like :" + SmartCombo.FILTER_PARAM_NAME + " order by " + (ReflectionUtilities.extendsOrImplements(lookupClass, LookupInt.class) ? "p.intValue" : "p.description"),
            "where p.id = :" + SmartCombo.FILTER_PARAM_NAME);
    convertToUpper = true;
  }

  public SmartCombo(String fieldName, Class<? extends IdentifiableSupport> identifiableClass , String propertyToList)  {
    this(fieldName,
            "select p.id, p."+propertyToList+" from " + identifiableClass.getName() + " as p",
            "where upper(p."+propertyToList+") like :" + SmartCombo.FILTER_PARAM_NAME + " order by " + propertyToList,
            "where p.id = :" + SmartCombo.FILTER_PARAM_NAME);
    convertToUpper = true;
  }


  @Override
public String getDiscriminator() {
    return SmartCombo.class.getName();
  }

  @Override
public boolean validate(PageState pageState) throws IOException, ServletException {
    return true;
  }

  @Override
public void toHtml(PageContext pageContext) {
    if (classic) {
      this.urlToInclude = "/commons/layout/smartCombo/partCombo.jsp";
    } else {
      if (shooted)
        throw new RuntimeException("You cannot call toHtml twice in case of smartCombo");
      shooted = true;

    }
    pageContext.getRequest().setAttribute(ACTION, DRAW_INPUT);
    super.toHtml(pageContext);
  }

  public void toHtmlI18n(PageContext pageContext) {
    PageState pageState = PageState.getCurrentPageState(pageContext);
    if (label == null)
      label = pageState.getI18n(fieldName);
    else  // added bicch 17/01/2008
      label = pageState.getI18n(label);
    toHtml(pageContext);
  }

  @Deprecated
  /**
   * @deprecated
   */
  public void addLinkToEntity(PageSeed pageSeed, String toolTip, PageState pageState) {
    addLinkToEntity(pageSeed, toolTip);
  }

  public void addLinkToEntity(PageSeed pageSeed, String toolTip) {
    linkToEntity = new ButtonLink(pageSeed);
    linkToEntity.additionalCssClass="linkToEntity";
    linkToEntity.iconChar="8";
    linkToEntity.toolTip = toolTip;
    linkToEntity.label="";
  }

  public static String getHiddenFieldName(String ceName) {
    return ceName + TEXT_FIELD_POSTFIX;
  }


  public List<Object[]> fillResultList(String filter, String hiddenValue) throws PersistenceException {
    OqlQuery oqlForFiltering = null;

    List<Object[]> prs = new ArrayList();

    // if there is any hidden value fill the first row
    if (JSP.ex(hiddenValue)&& !id.matches("(##[^#]*##)|(\\(#[^#]*#\\))")) {
      oqlForFiltering = new OqlQuery(hql + " " + whereForId);

      // modified R&P for PosgreSql

     // if (PersistenceConfiguration.getDefaultPersistenceConfiguration().dialect.equals(PostgreSQLDialect.class))
       if (PersistenceConfiguration.getDefaultPersistenceConfiguration().dialect.equals(org.hibernate.dialect.PostgreSQLDialectDBBlobs.class)){
         int intId=-1;
         try{
          intId = Integer.parseInt(hiddenValue);
         } catch (NumberFormatException nfe){}
         oqlForFiltering.getQuery().setInteger(SmartCombo.FILTER_PARAM_NAME, intId);
       }else
        oqlForFiltering.getQuery().setString(SmartCombo.FILTER_PARAM_NAME, JSP.w(hiddenValue) );

      //oqlForFiltering.setParameter(SmartCombo.FILTER_PARAM_NAME, hiddenValue);
      List<Object[]> list = oqlForFiltering.list();
      if(list.size()>0)
         prs.add(list.get(0));
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

    if (queryHelperForFiltering != null) {
      queryHelperForFiltering.setParameter(SmartCombo.FILTER_PARAM_NAME, (searchAll ? "%" + filter + "%" : filter + "%"));
      oqlForFiltering = queryHelperForFiltering.toHql();
    } else {
			if (whereForFiltering != null) {
				oqlForFiltering = new OqlQuery(hql + " " + whereForFiltering);
				oqlForFiltering.setParameter(SmartCombo.FILTER_PARAM_NAME,
						(searchAll ? "%" + filter + "%" : filter + "%"));
			} else {
				oqlForFiltering = new OqlQuery(hql);
			}

    }

    if (fixedParams.keySet() != null && fixedParams.keySet().size() > 0) {
      for (String s : fixedParams.keySet()) {

        Object value = fixedParams.get(s);
        if (value instanceof Collection)
          try {
            oqlForFiltering.getQuery().setParameterList(s, (Collection) value);
          } catch (HibernateException e) {
            throw new PersistenceException(e);
          }
        else
          oqlForFiltering.getQuery().setParameter(s, value);
      }
    }

    oqlForFiltering.getQuery().setMaxResults(maxRowToFetch);
    List<Object[]> listFromQuery = oqlForFiltering.list();


    // fill the result list if the line in not already there
    for (Object[] addLine : listFromQuery) {
      boolean found = false;
      for (Object[] existLine : alreadyThere) {
        if (addLine[0].equals(existLine[0])) {
          found = true;
          break;
        }
      }
      if (!found) {
        prs.add(addLine);
      }
    }


    return prs;

  }


  public String getTextValue(RestState restState) {
    String id = restState.getEntry(fieldName).stringValueNullIfEmpty();
    String ret="";

    //if there is an id, get the value
    if (JSP.ex(id) && !id.matches("(##[^#]*##)|(\\(#[^#]*#\\))")) { // avoid call when JST template is in action
      ret=getTextValue(id);
    } else if (addAllowed) { // if addAllowed must return _txt
      ret = JSP.w(restState.getEntry(fieldName + SmartCombo.TEXT_FIELD_POSTFIX).stringValueNullIfEmpty());
    }
    return ret;
  }


  public String getTextValue(String id) {
    String ret = "";
    try {
      OqlQuery oqlForId;

      oqlForId = new OqlQuery(hql + " " + whereForId);

      Object result = null;
      try {
        // modified R&P for PosgreSql on  Nov 11, 2008:
        if (PersistenceConfiguration.getDefaultPersistenceConfiguration().dialect.equals(org.hibernate.dialect.PostgreSQLDialectDBBlobs.class))
          //if (PersistenceConfiguration.getDefaultPersistenceConfiguration().dialect.equals(PostgreSQLDialect.class))
          oqlForId.getQuery().setInteger(SmartCombo.FILTER_PARAM_NAME, Integer.parseInt(id + ""));
        else
          oqlForId.getQuery().setString(SmartCombo.FILTER_PARAM_NAME, id + "");


        result = oqlForId.uniqueResultNullIfEmpty();
      } catch (Throwable e) {
        //this can happen when smartcombo is used in JST teplates when id -> ##id## or (#obj.id#)
        // Tracer.platformLogger.error(e);
      }

      if (result != null) {
        ret = ((Object[]) result)[columnToCopyInDescription] + "";
      }
    } catch (Throwable t) {
      //this can happen when smartcombo is used in JST teplates when id -> ##id## or (#obj.id#)
    }
    return ret;
  }
}
