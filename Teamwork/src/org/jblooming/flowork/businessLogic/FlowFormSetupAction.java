package org.jblooming.flowork.businessLogic;

import org.jblooming.flowork.FlowFields;
import org.jblooming.flowork.PlatformJbpmSessionFactory;
import org.jblooming.flowork.FieldAvailable;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.input.Collector;
import org.jblooming.waf.view.PageState;
import org.jbpm.graph.def.ProcessDefinition;
import org.jbpm.db.GraphSession;
import org.jbpm.JbpmContext;

import java.text.ParseException;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;


/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class FlowFormSetupAction {

  public void save(PageState pageState) throws PersistenceException, ActionException {
    pageState.initializeEntries("table");

    String stepName = pageState.getEntry("STEP_ID").stringValue();

    JbpmContext jbpmSession = PlatformJbpmSessionFactory.getJbpmContext(pageState);
    GraphSession gs = jbpmSession.getGraphSession();
    ProcessDefinition def = gs.loadProcessDefinition(Long.parseLong(pageState.mainObjectId.toString()));

    final String flowName = def.getName();
    FlowFields ff = (FlowFields) PersistenceHome.findUnique(FlowFields.class, "flowName", flowName);
    if (ff == null) {
      ff = new FlowFields();
      ff.setIdAsNew();
      ff.setFlowName(flowName);
    }
    TreeMap ch = Collector.chosen("CPF", pageState);
    Map selCb = Collector.selectedCheckBoxes("CPF", pageState);

    if (ch != null && ch.keySet().size() > 0) {
      boolean first = true;
      StringBuffer serVal = new StringBuffer();
      for (Object o : ch.keySet()) {
        String fieldName = (String) o;
        if (fieldName.trim().length() > 0) {
          if (!first)
            serVal.append(",");
          serVal.append(fieldName).append("__+__");
          if (selCb.get(fieldName) != null) {
            List selCheck = (List) selCb.get(fieldName);
            if (selCheck != null && selCheck.contains(FlowFields.MANDATORY_FIELD))
              serVal.append(FlowFields.MANDATORY_FIELD).append("__+__");
            if (selCheck != null && selCheck.contains(FlowFields.READ_ONLY_FIELD))
              serVal.append(FlowFields.READ_ONLY_FIELD).append("__+__");
          }
          first = false;
        }
      }
      ff.putNodeFields(stepName, serVal.toString());
    } else {
      ff.removeTokenFieldsByKey(stepName);
    }
    String stepDescr = pageState.getEntry("STEP_DESC").stringValueNullIfEmpty();
    if (stepDescr!= null && stepDescr.length()>0)
      ff.getNodeDescriptions().put(stepName, stepDescr);
    else
      ff.getNodeDescriptions().remove(stepName);
    ff.store();
  }

  public void edit(PageState pageState) throws PersistenceException, ActionException {


    String stepName = pageState.getEntry("STEP_ID").stringValue();

    JbpmContext jbpmSession = PlatformJbpmSessionFactory.getJbpmContext(pageState);
    GraphSession gs = jbpmSession.getGraphSession();
    ProcessDefinition def = gs.loadProcessDefinition(Long.parseLong(pageState.mainObjectId.toString()));

    final String flowName = def.getName();
    TreeMap<String,String> sel = new TreeMap<String,String>();
    FlowFields ff = (FlowFields) PersistenceHome.findUnique(FlowFields.class, "flowName", flowName);
    if (ff != null) {
      String serVal = ff.getTokenFields(stepName);

      if (serVal != null) {
        List<String> fields = StringUtilities.splitToList(serVal, ",");
        if (fields != null && fields.size() > 0) {
          for (String fieldSer  : fields) {
            List<String> fieldAttr = StringUtilities.splitToList(fieldSer, "__+__");
            sel.put(fieldAttr.get(0), StringUtilities.deCamel(fieldAttr.get(0)));
            if (fieldAttr.size() > 1 && fieldAttr.get(1) != null)
              Collector.addSelectedCheckBox("CPF", fieldAttr.get(0), fieldAttr.get(1), pageState);
            if (fieldAttr.size() > 2 && fieldAttr.get(1) != null)
              Collector.addSelectedCheckBox("CPF", fieldAttr.get(0), fieldAttr.get(2), pageState);
          }
        }
      }
      //make
      pageState.addClientEntry("STEP_DESC",ff.getNodeDescriptions().get(stepName));
    }

    TreeMap<String,String> cand = new TreeMap<String,String>();

    List<FieldAvailable> dfs = new OqlQuery("from " + FieldAvailable.class.getName() + " as df order by df.name").list();
    for (FieldAvailable df : dfs) {
      if (sel.get(df.getName()) == null)
        cand.put(df.getName(), df.getLabel() != null ? df.getLabel() : StringUtilities.deCamel(df.getName()));
    }
    //make
    Collector.make("CPF", cand, sel, pageState);
  }


}
