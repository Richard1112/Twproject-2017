package org.jblooming.waf.html.input;

import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.view.ClientEntries;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.view.RestState;

import javax.servlet.ServletException;
import java.io.IOException;
import java.util.*;

public class Selector extends JspHelper implements HtmlBootstrap {
  public Form form;
  public String height;
  public String label;
  public boolean selectedOnTop=false;
  public boolean disabled = false;

  public TreeMap checkBoxes;
  public TreeMap radioButtons;

  public boolean orderById = false;

  public static final String FLD_HIDDEN_ID = "_HID";
  public static final String FLD_KEY = "_KEY";
  public static final String FLD_CHECK_ADDITIONAL = "F_C_A_";
  public static final String FLD_RADIO_ADDITIONAL = "F_R_A_";
  public static final String DRAW_THEREST = "DRTR";
  public static final String DRAW_SELECTED ="DRSE";


  public Selector(String id, Form form) {
    this.id = id;
    urlToInclude = "/commons/layout/partSelector.jsp";
    this.form = form;
  }

  public String getDiscriminator() {
    return this.getClass().getName();
  }

  public boolean validate(PageState pageState) throws IOException, ServletException {
    return true;
  }

  public static String getHiddenPrefix(String id) {
    return FLD_HIDDEN_ID + id;
  }

  public static String getCheckPrefix(String id) {
    return FLD_KEY + id;
  }

  public static String getCheckBoxName(String key) {
    if (FLD_HIDDEN_ID.length()<key.length())
      return FLD_KEY + key.substring(FLD_HIDDEN_ID.length());
    else
      return null;
  }

  public static void make(String id, TreeMap<String,String> candidates, TreeMap<String,String> chosen, RestState pageState) {
    make(id, candidates, chosen, false, pageState);
  }

  public static void make(String id, TreeMap<String,String> candidates, TreeMap<String,String> chosen, boolean radioButtons, RestState pageState) {
    if (candidates != null && candidates.size() > 0) {
      for (Iterator iterator = candidates.keySet().iterator(); iterator.hasNext();) {
        Object key = iterator.next();
        pageState.addClientEntry(getHiddenPrefix(id) + key, candidates.get(key).toString());  // _HIDspNorme12 ,  Nano alla forca
      }
    }
    if (chosen != null && chosen.size() > 0) {
      for (Iterator iterator = chosen.keySet().iterator(); iterator.hasNext();) {
        Object key = iterator.next();
        if (!candidates.containsKey(key)) {
          // hidden field with value
          pageState.addClientEntry(getHiddenPrefix(id) + key, chosen.get(key).toString());
          if (radioButtons) {
            // radio selected
            pageState.addClientEntry(getCheckPrefix(id) + key, "checked");
          } else {
            // checkbox selected
            pageState.addClientEntry(getCheckPrefix(id) + key, "on");
          }
        }
      }
    }
  }

  /**
   * Makes a snapshot of current candidates and chosen of the selector of id <code>id</code> returning them
   * collected in TreeMap[0],TreeMap[1] respectively.
   */
  public static TreeMap<String, String>[] snapShot(String id, RestState pageState) {
    TreeMap<String, String> cands = new TreeMap<String, String>();
    TreeMap<String, String> chosen = new TreeMap<String, String>();
    ClientEntries ces = pageState.getClientEntries();
    if (ces.getEntryKeys() != null) {
      Iterator i = new HashSet(ces.getEntryKeys()).iterator();
      while (i.hasNext()) {
        String key = (String) i.next();
        String checkBoxName = Selector.getCheckBoxName(key);
        if (key.startsWith(Selector.FLD_HIDDEN_ID + id)) {
          if (pageState.getEntry(checkBoxName).stringValueNullIfEmpty() == null)
            cands.put(key.substring((Selector.FLD_HIDDEN_ID + id).length()), ces.getEntry(key).stringValueNullIfEmpty());
          else
            chosen.put(key.substring((Selector.FLD_HIDDEN_ID + id).length()), ces.getEntry(key).stringValueNullIfEmpty());

        }
      }
    }
    return new TreeMap[]{cands, chosen};
  }

  public static TreeMap<String, String> chosen(String id, RestState pageState) {
    return snapShot(id, pageState)[1];
  }

  /**
   * @return a map with as keys the id of the chosen element, as value a list of its selected checkboxes
   */
  public static Map selectedCheckBoxes(String selectorId, RestState pageState) {
    TreeMap selChecks = new TreeMap();
    Set ids = snapShot(selectorId, pageState)[1].keySet();
    ClientEntries ces = pageState.getClientEntries();
    if (ces.getEntryKeys() != null) {
      Iterator i = new HashSet(ces.getEntryKeys()).iterator();
      while (i.hasNext()) {
        String key = (String) i.next();
        if (key.startsWith(Selector.FLD_CHECK_ADDITIONAL + selectorId)) {
          String idAndAddFld = key.substring((Selector.FLD_CHECK_ADDITIONAL + selectorId).length());
          //find id
          if (ids != null && ids.size() > 0) {
            for (Iterator iterator = ids.iterator(); iterator.hasNext();) {
              String id = (String) iterator.next();
              if (idAndAddFld.startsWith(id)) {
                List checkedForId = (List) selChecks.get(id);
                if (checkedForId == null)
                  checkedForId = new ArrayList();
                checkedForId.add(idAndAddFld.substring(id.length()));
                selChecks.put(id, checkedForId);
              }
            }
          }
        }
      }
    }
    return selChecks;
  }

  /**
   * @return a map with as keys the id of the chosen element, as value the value of the selected radio
   */
  public static Map selectedRadios(String selectorId, RestState pageState) {
    TreeMap selChecks = new TreeMap();
    Set ids = snapShot(selectorId, pageState)[1].keySet();
    ClientEntries ces = pageState.getClientEntries();
    if (ces.getEntryKeys() != null) {
      Iterator i = new HashSet(ces.getEntryKeys()).iterator();
      while (i.hasNext()) {
        String key = (String) i.next();
        if (key.startsWith(Selector.FLD_RADIO_ADDITIONAL)) {
          String idAndAddFld = key.substring(Selector.FLD_RADIO_ADDITIONAL.length());
          //find id
          if (ids != null && ids.size() > 0) {
            for (Iterator iterator = ids.iterator(); iterator.hasNext();) {
              String id = (String) iterator.next();
              if (idAndAddFld.startsWith(selectorId + id)) {
                selChecks.put(id, ces.getEntry(key).stringValueNullIfEmpty());
              }
            }
          }
        }
      }
    }
    return selChecks;
  }

  public static void addSelectedCheckBox(String id, String rowId, String checkBoxName, RestState pageState) {
    pageState.addClientEntry(getCandidateCheckAddPrefix(id) + rowId + checkBoxName, "on");

  }

  public static String getCandidateCheckAddPrefix(String id) {
    return FLD_CHECK_ADDITIONAL + id;
  }

  public static void addSelectedRadios(String id, String rowId, String radioName, RestState pageState) {
    //String key = getCandidateRadioAddPrefix(id) + rowId + radioName;
    //pageState.addClientEntry(key, "checked");
    String key = getCandidateRadioAddPrefix(id) + rowId;
    pageState.addClientEntry(key, radioName);
  }

  public static String getCandidateRadioAddPrefix(String id) {
    return FLD_RADIO_ADDITIONAL + id;
  }

  public static String getCheckVal(String idCollector) {
    return FLD_KEY + idCollector;
  }

 /* clear the selector  */
  public void clearClientEntry(String selectorId, RestState pageState) {
      ClientEntries oldEntries = pageState.getClientEntries();
      ClientEntries newEntries = new ClientEntries();
      if (oldEntries != null && oldEntries.size() > 0) {
        for (Iterator<String> iterator = oldEntries.getEntryKeys().iterator(); iterator.hasNext();) {
          String key = iterator.next();
          if (!key.startsWith(Selector.getHiddenPrefix(selectorId)) && !key.startsWith(Selector.getCheckPrefix(selectorId))) {
            ClientEntry entry = oldEntries.getEntry(key);
            newEntries.addEntry(entry);
          }

        }
      }
      pageState.setClientEntries(newEntries);
    }

  /* clear the chosen list of selector  */ 
    public void clearChosen(String selectorId, RestState pageState) {
      TreeMap<String, String> chosen = Selector.chosen(selectorId, pageState);
      TreeMap<String, String> all = Selector.snapShot(selectorId, pageState)[0];
      clearClientEntry(selectorId, pageState);
      TreeMap<String, String> ctm = new TreeMap<String, String>();
      TreeMap<String, String> candTm = new TreeMap<String, String>();

      boolean found = false;
      for (String allKey : all.keySet()) {
        for (String allChosen : chosen.keySet()) {
          if (allKey.equals(allChosen)) {
            found = true;
          }
        }
        if (found == false) {
          candTm.put((allKey), all.get(allKey));
          found = false;
        }
      }
      Selector.make(selectorId, candTm, ctm, pageState);
    }

}
