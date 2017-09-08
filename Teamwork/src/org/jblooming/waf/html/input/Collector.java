package org.jblooming.waf.html.input;

import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.core.JspIncluder;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.view.*;
import org.jblooming.tracer.Tracer;

import javax.servlet.ServletException;
import java.io.IOException;
import java.util.*;

/**
 * Handles collections associations. Relies on template
 * <p/>
 * <a href="/commons/layout/partCollector.jsp">partCollector.jsp</a>
 * <p/>
 * There is a complete example of usage in
 * <p/>
 * <a href="/test/testCollector.jsp">/test/testCollector.jsp</a>
 *
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class Collector extends JspHelper implements HtmlBootstrap {

  public Form form;
  public int height=200;

  public TreeMap checkBoxes;

  public static final String DRAW_CANDIDATES = Fields.FORM_PREFIX + "DCND";
  public static final String DRAW_CHOSEN = Fields.FORM_PREFIX + "DCHS";

  // these do not rely on

  public String CANDIDATES_LABEL = "Candidates";
  public String CHOSEN_LABEL = "Chosen";
  public String NO_CANDIDATES = "No candidates";
  public String NO_CHOSEN = "No chosen ones";

  public String MOVE_ALL_TO_SELECTED_LABEL = ">>";
  public String MOVE_ALL_TO_SELECTED_TITLE = "Move all to chosen";
  public String MOVE_TO_SELECTED_LABEL = ">";
  public String MOVE_TO_SELECTED_TITLE = "Move selected to chosen";
  public String MOVE_TO_UNSELECTED_LABEL = "<";
  public String MOVE_TO_UNSELECTED_TITLE = "Move selected to candidates";
  public String MOVE_ALL_TO_UNSELECTED_LABEL = "<<";
  public String MOVE_ALL_TO_UNSELECTED_TITLE = "Move all to candidates";
  public String SYNCHRONIZE_LABEL = "<>";
  public String SYNCHRONIZE_TITLE = "Synchonize";

  public static final String FLD_CAND_HIDDEN_ID = Fields.FORM_PREFIX + "CA_HID";
  public static final String FLD_CHS_HIDDEN_ID = Fields.FORM_PREFIX + "CH_HID";
  public static final String FLD_CAND_KEY = Fields.FORM_PREFIX + "CND_KEY";
  public static final String FLD_CHOSEN_KEY = Fields.FORM_PREFIX + "CHS_KEY";
  public static final String FLD_CHECK_ADDITIONAL = Fields.FORM_PREFIX + "F_C_A";
  public static final String FLD_RADIO_ADDITIONAL = Fields.FORM_PREFIX + "F_R_A";
  /**
   * needed to change left side of collector with own jspIncluder
   */
  public String customUrlToInclude;

  public boolean disabled = false;

  // --- add
  public JspIncluder jspIncluderChosen;
  public JspIncluder jspIncluderCandidates;

  public Collector(String id, int height, Form form) {
    this.id = id;
    urlToInclude = "/commons/layout/partCollector.jsp";
    this.form = form;
    this.height = height;
  }

  public String getDiscriminator() {
    return this.getClass().getName();
  }

  public boolean validate(PageState pageState) throws IOException, ServletException {
    return true;
  }

  /**
   * Moves according to the action the selected ones as client entries to each side
   * altering the the TreeMap pair {candidates, chosen}
   *
   * @param pageState
   */
  public static void move(String id, RestState pageState) {
    String command = pageState.getCommand();
    ClientEntries ces = pageState.getClientEntries();
    String candidateFieldPrefix = Collector.getCandidateFieldPrefix(id);
    String chosenFieldPrefix = Collector.getChosenFieldPrefix(id);

    if ((Commands.MOVE_ALL_TO_SELECTED + id).equals(command))
      moveAll(candidateFieldPrefix, chosenFieldPrefix, ces, pageState);
    else if ((Commands.MOVE_TO_SELECTED + id).equals(command)) {
      moveTo(id, FLD_CHOSEN_KEY, chosenFieldPrefix, candidateFieldPrefix, ces, pageState);
      //move sel chosen to cands
    } else if ((Commands.MOVE_TO_UNSELECTED + id).equals(command)) {
      moveTo(id, FLD_CAND_KEY, candidateFieldPrefix, chosenFieldPrefix, ces, pageState);
    } else if ((Commands.SYNCHRONIZE + id).equals(command)) {
      synchronize(id, pageState);
    } else if ((Commands.MOVE_ALL_TO_UNSELECTED + id).equals(command))
      moveAll(chosenFieldPrefix, candidateFieldPrefix, ces, pageState);
  }

  public static void synchronize(String id, RestState pageState) {
    moveTo(id, FLD_CHOSEN_KEY, Collector.getChosenFieldPrefix(id), Collector.getCandidateFieldPrefix(id), pageState.getClientEntries(), pageState);
    moveTo(id, FLD_CAND_KEY, Collector.getCandidateFieldPrefix(id), Collector.getChosenFieldPrefix(id), pageState.getClientEntries(), pageState);
  }

  private static void moveTo(String id, String fldKey, String newPrefix, String oldPrefix, ClientEntries ces, RestState pageState) {

    if (ces.getEntryKeys() != null) {
      Iterator i = new HashSet(ces.getEntryKeys()).iterator();

      while (i.hasNext()) {
        String key = (String) i.next();
        if (key.startsWith(fldKey + id)) {
          String keyMap = newPrefix + key.substring((fldKey + id).length());
          String oldEntry = oldPrefix + keyMap.substring((fldKey + id).length() - 1);
          pageState.addClientEntry(keyMap, ces.getEntry(oldEntry).stringValueNullIfEmpty());
          ces.deleteEntry(oldEntry);
        }
      }
    }
  }

  private static void moveAll(String oldField, String newField, ClientEntries ces, RestState pageState) {
    if (ces.getEntryKeys() != null) {
      Iterator i = new HashSet(ces.getEntryKeys()).iterator();

      while (i.hasNext()) {
        String key = (String) i.next();
        if (key.startsWith(oldField)) {
          pageState.addClientEntry(newField + key.substring(oldField.length()), ces.getEntry(key).stringValueNullIfEmpty());
          ces.deleteEntry(key);
        }
      }
    }
  }

  public static boolean isCollectorCommand(String id, String cmd) {
    return (Commands.MOVE_ALL_TO_SELECTED + id).equals(cmd) ||
            (Commands.MOVE_TO_SELECTED + id).equals(cmd) ||
            (Commands.MOVE_TO_UNSELECTED + id).equals(cmd) ||
            (Commands.MOVE_ALL_TO_UNSELECTED + id).equals(cmd) ||
            (Commands.SYNCHRONIZE + id).equals(cmd);
  }

  public static String getCandidateFieldPrefix(String id) {
    return FLD_CAND_HIDDEN_ID + id;
  }

  public static String getChosenFieldPrefix(String id) {
    return FLD_CHS_HIDDEN_ID + id;
  }

  public static String getCandidateCheckValue(String key) {
    return FLD_CAND_KEY + key.substring(FLD_CHS_HIDDEN_ID.length());
  }

  public static String getCandidateCheckAddPrefix(String id) {
    return FLD_CHECK_ADDITIONAL + id;
  }

  public static void make(String id, TreeMap candidates, TreeMap chosen, PageSeed pageState) {

    if (candidates != null && candidates.size() > 0) {
      for (Iterator iterator = candidates.keySet().iterator(); iterator.hasNext();) {
        Object key = iterator.next();
        Object c = candidates.get(key);
        if (c!=null)
          pageState.addClientEntry(getCandidateFieldPrefix(id) + key, c.toString());
        else
          Tracer.platformLogger.error("candidates value null for: "+key);
      }
    }
    
    if (chosen != null && chosen.size() > 0) {
      for (Iterator iterator = chosen.keySet().iterator(); iterator.hasNext();) {
        Object key = iterator.next();
        if (chosen.get(key)!=null)
          pageState.addClientEntry(getChosenFieldPrefix(id) + key, chosen.get(key).toString());
      }
    }

  }


  public static void clearClientEntry(String collectorID, RestState pageState) {
    ClientEntries oldEntries = pageState.getClientEntries();
    ClientEntries newEntries = new ClientEntries();
    if (oldEntries != null && oldEntries.size() > 0) {
      for (Iterator<String> iterator = oldEntries.getEntryKeys().iterator(); iterator.hasNext();) {
        String key = iterator.next();
        if (!key.startsWith(Collector.getCandidateFieldPrefix(collectorID))) {
          ClientEntry entry = oldEntries.getEntry(key);
          newEntries.addEntry(entry);
        }
      }
    }
    pageState.setClientEntries(newEntries);
  }

  /**
   * Makes a snapshot of current candidates and chosen of the Collector of id <code>id</code> returning them
   * collected in TreeMap[0],TreeMap[1] respectively.
   */
  public static TreeMap<String,String>[] snapShot(String id, PageSeed pageState) {

    TreeMap<String,String> cands = new TreeMap<String,String>();
    TreeMap<String,String> chosen = new TreeMap<String,String>();
    ClientEntries ces = pageState.getClientEntries();
    if (ces.getEntryKeys() != null) {
      Iterator i = new HashSet(ces.getEntryKeys()).iterator();
      while (i.hasNext()) {
        String key = (String) i.next();
        if (key.startsWith(Collector.FLD_CAND_HIDDEN_ID + id))
          cands.put(key.substring((Collector.FLD_CAND_HIDDEN_ID + id).length()), ces.getEntry(key).stringValueNullIfEmpty() != null ? ces.getEntry(key).stringValueNullIfEmpty() : "");
        else if (key.startsWith(Collector.FLD_CHS_HIDDEN_ID + id)) {
          chosen.put(key.substring((Collector.FLD_CHS_HIDDEN_ID + id).length()), ces.getEntry(key).stringValueNullIfEmpty() != null ? ces.getEntry(key).stringValueNullIfEmpty() : "");
        }
      }
    }
    return new TreeMap[]{cands, chosen};
  }

  /**
   * @param id the collector name
   * @return a tree map where the key is the hidden value, the value the shown one
   */
  public static TreeMap<String,String> chosen(String id, PageSeed pageState) {
    return snapShot(id, pageState)[1];
  }

  /**
   * @param id the collector name
   * @return a tree map where the key is the hidden value, the value the shown one
   */
  public static TreeMap<String,String> candidates(String id, PageSeed pageState) {
    return snapShot(id, pageState)[0];
  }

  /**
   * @return a map with as keys the id of the chosen element, as value a list of its selected checkboxes
   */
  public static Map<String,List<String>> selectedCheckBoxes(String collectorId, PageSeed pageState) {
    TreeMap<String,List<String>> selChecks = new TreeMap<String,List<String>>();
    Set ids = snapShot(collectorId, pageState)[1].keySet();
    ClientEntries ces = pageState.getClientEntries();
    if (ces.getEntryKeys() != null) {
      Iterator i = new HashSet(ces.getEntryKeys()).iterator();
      while (i.hasNext()) {
        String key = (String) i.next();
        if (key.startsWith(Collector.FLD_CHECK_ADDITIONAL + collectorId)) {
          String idAndAddFld = key.substring((Collector.FLD_CHECK_ADDITIONAL + collectorId).length());
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

  public void setDefaultLabels(RestState pageState) {
    this.MOVE_ALL_TO_SELECTED_TITLE = pageState.getI18n("MOVE_ALL_TO_SELECTED_TITLE");
    this.MOVE_TO_SELECTED_TITLE = pageState.getI18n("MOVE_TO_SELECTED_TITLE");
    this.MOVE_TO_UNSELECTED_TITLE = pageState.getI18n("MOVE_TO_UNSELECTED_TITLE");
    this.MOVE_ALL_TO_UNSELECTED_TITLE = pageState.getI18n("MOVE_ALL_TO_UNSELECTED_TITLE");

    this.NO_CANDIDATES = pageState.getI18n("NO_CANDIDATES");
    this.NO_CHOSEN = pageState.getI18n("NO_CHOSEN");

    this.CANDIDATES_LABEL = pageState.getI18n("CANDIDATES_LABEL");
    this.CHOSEN_LABEL = pageState.getI18n("CHOSEN_LABEL");
  }

  public static void addSelectedCheckBox(String id, String rowId, String checkBoxName, RestState pageState) {
    pageState.addClientEntry(getCandidateCheckAddPrefix(id) + rowId + checkBoxName, "on");
  }

  public static void preserveChosen(String collectorName, Form f, RestState pageState) {
    f.url.getClientEntries().addEntries(pageState.getClientEntries().getEntriesStartingWithStripped(Collector.getChosenFieldPrefix(collectorName)).values());
  }
}