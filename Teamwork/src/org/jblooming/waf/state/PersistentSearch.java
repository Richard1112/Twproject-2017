package org.jblooming.waf.state;

import org.jblooming.operator.Operator;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.view.ClientEntries;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import java.util.*;

public class PersistentSearch {

  private String category;
  private String name;
  private Map entries = new Hashtable();
  protected final static String METASEPARATOR = "__x__";
  protected final static String SEPARATOR = "__+__";
  protected final static String SEPARATORPAIR = "__$__";
  public static final String PERSISTENT_SEARCH = "PRSSRC";

  public Map getEntries() {
    return entries;
  }

  public String getCategory() {
    return category;
  }

  public void setCategory(String category) {
    this.category = category;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }


  private String getFlatCopy() {

    StringBuffer value = new StringBuffer();
    if (entries.keySet() != null && entries.keySet().size() > 0) {
      for (Iterator iterator = entries.keySet().iterator(); iterator.hasNext();) {
        String key = (String) iterator.next();
        value.append(key).append(SEPARATOR).append(entries.get(key)).append(SEPARATORPAIR);
      }
    }
    return category + METASEPARATOR + name + METASEPARATOR + value.toString();
  }

  private static void feedPageState(String data, RestState pageState) {
    String[] codes = StringUtilities.splitToArray(data, METASEPARATOR);
    final String value = codes[2];
    String[] pairs = StringUtilities.splitToArray(value, SEPARATORPAIR);
    for (int j = 0; j < pairs.length; j++) {
      String pair = pairs[j];
      if (pair != null && pair.length() > 0) {
        String[] cv = StringUtilities.splitToArray(pair, SEPARATOR);
        if (cv.length == 2)
          pageState.addClientEntry(cv[0], cv[1]);
      }
    }

  }

  public static boolean feedFromSavedSearch(RestState pageState) {
    boolean fed = false;
    Operator operator = pageState.getLoggedOperator();
    if (operator != null) {
      String filterCategory = pageState.getEntry(Fields.FLD_FILTER_CATEGORY).stringValueNullIfEmpty();
      String filterName = JSP.w(pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty());
      boolean isSelectedFromCombo = pageState.getEntry("FLNMSEL").checkFieldValue();
      String command = pageState.getCommand();

      if (JSP.ex(filterName)) {
        String filterCategoryName = JSP.w(filterCategory) + filterName;
        if (isSelectedFromCombo) {

          String data = operator.getFilter(filterCategoryName);
          if (JSP.ex(data)) {

            // should reset ce before apply the filter  Roberto fecit 5/Dec/2008
            pageState.setClientEntries(new ClientEntries());

            feedPageState(data, pageState);

            // add filter client entries
            pageState.addClientEntry(Fields.FLD_FILTER_NAME, filterName);
            pageState.addClientEntry(Fields.FLD_FILTER_CATEGORY, filterCategory);

            fed = true;
          }
        }
      }
    }
    return fed;
  }

  public static boolean feedFromDefaultSearch(String category, RestState pageState) {
    boolean found = false;
    Operator operator = pageState.getLoggedOperator();
    if (operator != null) {
      if (pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty() == null) {
        Map<String, String> filters = operator.getFilters();
        for (String fn : filters.keySet()) {
          if (fn.toUpperCase().startsWith((category + "d:").toUpperCase())) {
            pageState.addClientEntry(Fields.FLD_FILTER_NAME, fn.substring(category.length()));
            pageState.addClientEntry(Fields.FLD_FILTER_CATEGORY, category);
            pageState.addClientEntry("FLNMSEL", Fields.TRUE);
            found = true;
            break;
          }
        }
      }
    }
    return found;
  }


  /**
   * @param category
   * @param pageState
   * @throws PersistenceException
   */
  public static void saveSearch(String category, PageState pageState) throws PersistenceException {

    Operator operator = pageState.getLoggedOperator();
    if (operator != null) {

      String newFilterName = JSP.w(pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty());

      if (JSP.ex(newFilterName)) {

        // added in order to avoid filter reset robicch 26/04/2009
        //if (!JSP.ex(operator.getFilter(category + newFilterName))) {

          PersistentSearch pf = new PersistentSearch();
          pf.setName(category + newFilterName);
          pf.setCategory(category);

          final ClientEntries clientEntries = pageState.getClientEntries();
          for (String key : clientEntries.getEntryKeys()) {


            String value = clientEntries.getEntry(key).stringValueNullIfEmpty();
            if (!Fields.FLD_FILTER_NAME.equals(key) && value != null && !Fields.FLD_FILTER_CATEGORY.equals(key) && !"FLNMSEL".equals(key)) {
              pf.entries.put(key, value);
            }

          }

          operator.getFilters().put(pf.getName(), pf.getFlatCopy());
          operator.store();
        }

      //}
    }
  }

}

