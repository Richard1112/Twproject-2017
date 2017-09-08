package org.jblooming.designer.businessLogic;

import org.jblooming.designer.*;
import org.jblooming.persistence.exceptions.*;
import org.jblooming.waf.view.*;
import org.jblooming.waf.html.input.*;
import org.jblooming.waf.exceptions.*;
import org.jblooming.utilities.*;
import org.jblooming.ontology.*;
import org.jblooming.tracer.Tracer;

import java.util.*;
import java.text.*;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */

public class DesignerAction {


  public void cmdSave(Designer designer, PageState pageState) throws PersistenceException {
    pageState.initializeEntries("table");

    DesignerData designerData = designer.getDesignerData();
    Map<String, DesignerField> designerFields = designer.designerFields;

    if (designerFields != null && designerFields.size() > 0) {
      for (String key : designerFields.keySet()) {
        DesignerField df = designerFields.get(key);
        if (df != null) {
          if (df instanceof Detail) {
            Detail detail = (Detail) df;
            if(!detail.readOnly) {
              HashSet<Integer> ids = new HashSet<Integer>();
              Map<String, ClientEntry> mces = pageState.getClientEntries().getEntriesStartingWithStripped( detail.name + "_");
              for (String key2 : mces.keySet()) {
                String[] sequ = key2.split("_");
                if (sequ.length == 2) {
                  String id = key2.substring(key2.lastIndexOf("_") + 1, key2.length());

                  try {
                    ids.add(Integer.parseInt(id));
                  } catch (NumberFormatException e) {
                    //e.printStackTrace();
                    Tracer.platformLogger.warn("Got invalid id on DesignerAction cmdSave:"+id);
                  }
                }
              }
              HashSet<Integer> idsToRemove = new HashSet<Integer>();
              if (designer.getDesignerData() != null && designer.getDesignerData().getValueMap().size() > 0) {
                for (String mapKey : designer.getDesignerData().getValueMap().keySet()) {
                  if (mapKey.lastIndexOf(detail.name + "_") > -1) {
                    String mapIdString = mapKey.substring(mapKey.lastIndexOf("_") + 1, mapKey.length());
                    try {
                      int mapId = Integer.parseInt(mapIdString);
                      if (!ids.contains(mapId))
                        idsToRemove.add(mapId);
                    } catch (NumberFormatException e) {
                      //e.printStackTrace();
                      Tracer.platformLogger.warn("Got invalid id on DesignerAction cmdSave-remove:"+mapIdString);  
                    }
                  }
                }
              }
              if(idsToRemove != null && idsToRemove.size()>0){
                for(int mapId : idsToRemove) {
                  removeDetailLineFromDB(detail, pageState, mapId, designerData);
                }
              }
              if (ids != null && ids.size() > 0) {
                if (detail.detailDesignerFields != null && detail.detailDesignerFields.size() > 0) {
                  for (int id : ids) {
                    boolean allIsEmpty = true;
                    for (String key2 : detail.detailDesignerFields.keySet()) {
                      DesignerField detail_df = detail.detailDesignerFields.get(key2);
                      if (detail_df != null) {
                        boolean filled = designerData.putValue(pageState, detail_df,  detail.name + "_" + detail_df.name + "_" + id, detail.name + "_" + detail_df.name + "_" + id);
                        if (filled)
                          allIsEmpty = false;
                      }
                    }
                    if (allIsEmpty) {
                      removeDetailLineFromDB(detail, pageState, id, designerData);
                    }
                  }
                }
              }
              if (detail.required) {
                Map<String, ClientEntry> mces2 = pageState.getClientEntries().getEntriesStartingWithStripped( detail.name + "_");
                if (mces2 == null || mces2.size() == 0) {
                  ClientEntry ce = pageState.getEntry("DETAIL_IS_EMPTY_" + detail.name);
                  ce.name = "DETAIL_IS_EMPTY_" + detail.name;
                  ce.errorCode = pageState.getI18n("DETAIL_IS_EMPTY");
                  pageState.addClientEntry(ce);
                }
              }
            }
          } else {
            designerData.putValue(pageState, df,  key, key);
          }
        }
      }
    }
    if (pageState.getClientEntries().validEntries())   {

      designerData.store();

      if (designerData.isNew())
        pageState.addMessageOK(pageState.getI18n("DOCUMENT_CORRECTLY_CREATED"));
      else
        pageState.addMessageOK(pageState.getI18n("DOCUMENT_CORRECTLY_SAVED"));

    }

    pageState.setMainObject(designerData);
    pageState.setMainObjectId(designer.referenceId); // ATTENZIONE MAINOBJECT E MAINOBJECTID DISALLINEATI
  }

  private void removeDetailLineFromDB(Detail detail, PageState pageState, int id, DesignerData designerData) {
    for (String key2 : detail.detailDesignerFields.keySet()) {
      DesignerField detail_df = detail.detailDesignerFields.get(key2);
      try {
        Class type = Class.forName(detail_df.kind);
        List classes = ReflectionUtilities.getInheritedClasses(type);
        if (detail_df.smartCombo != null || classes.contains(LookupSupport.class) || classes.contains(Identifiable.class))
          pageState.getClientEntries().deleteEntry( detail.name + "_" + key2 + "_" + id + SmartCombo.TEXT_FIELD_POSTFIX);
      } catch (ClassNotFoundException e) {
        Tracer.platformLogger.error(e);
      }
      pageState.getClientEntries().deleteEntry( detail.name + "_" + key2 + "_" + id);
      designerData.getValueMap().remove(detail.name + "_" + key2 + "_" + id);
    }
  }


  public void cmdEdit(Designer designer, PageState pageState) throws PersistenceException, ActionException {
    
    DesignerData designerData = designer.getDesignerData();
    pageState.setMainObject(designerData);
    pageState.setMainObjectId(designer.referenceId);  // ATTENZIONE MAINOBJECT E MAINOBJECTID DISALLINEATI
    if(!designer.readOnly) {  // this cycle updates loggable fields all times
      if (designer.designerFields != null && designer.designerFields.size() > 0) {
        for (String name : designer.designerFields.keySet()) {
          DesignerField df = designer.designerFields.get(name);
          if (df instanceof DesignerLoggable)
            createClientEntryForDesignerLoggable(designerData, (DesignerLoggable) df, name, pageState);
          else if (df instanceof Detail) {
            Detail detail = (Detail) df;
            if (detail.detailDesignerFields != null && detail.detailDesignerFields.size() > 0) {
              for (String detailName : detail.detailDesignerFields.keySet()) {
                DesignerField dfDetail = detail.detailDesignerFields.get(detailName);
                if (dfDetail instanceof DesignerLoggable)
                  createClientEntryForDesignerLoggable(designerData, (DesignerLoggable) dfDetail, name, pageState);
              }
            }
          } else {
            createClientEntryForDesignerField(designerData, df, name, pageState);
          }
        }
      }
    }
    if (designerData.getValueMap() != null && designerData.getValueMap().size() > 0) {
      for (String key : designerData.getValueMap().keySet()) {
        DesignerField df = null;
        if (designer.designerFields.containsKey(key)) {
          df = designer.designerFields.get(key);
        } else {
          String[] strings = key.split("_");  //vedere se c'è un modo più furbo per riconoscere le ce relative ai detail!!!!

          Detail detail = (Detail) designer.designerFields.get(strings[0]);
          if (detail != null)
            df = detail.detailDesignerFields.get(strings[1]);
        }
        if (df != null) {
          if (designer.readOnly || !(df instanceof DesignerLoggable))
            createClientEntryForDesignerField(designerData, df, key, pageState);
        }
      }
    }
  }

  private void createClientEntryForDesignerLoggable(DesignerData designerData, DesignerLoggable dl, String ceName, PageState pageState) throws PersistenceException, ActionException {
    if (dl != null) {
      String value = null;
      if (designerData.getValueMap().containsKey(ceName))
        value = designerData.getValueMap().get(ceName);
      else if (dl.initialValue != null) {
        value = dl.initialValue;
      } else {
        if (DesignerLoggable.LogType.LAST_MODIFIED.equals(dl.type))
          value = DateUtilities.dateToString(new Date(), "yyyy-MM-dd-HH-mm-ss");
        else if (DesignerLoggable.LogType.LAST_MODIFIER.equals(dl.type))
          value = pageState.getLoggedOperator().getFullname();
        else if (DesignerLoggable.LogType.CREATION_DATE.equals(dl.type))
          value = DateUtilities.dateToString(new Date(), "yyyy-MM-dd-HH-mm-ss");
        else if (DesignerLoggable.LogType.CREATOR.equals(dl.type))
          value = pageState.getLoggedOperator().getFullname();
      }
      if (Date.class.getName().equals(dl.kind)) {
        if (JSP.ex(value)) {
          Date d;
          try {
            d = DateUtilities.dateFromString(value, "yyyy-MM-dd-HH-mm-ss");
          } catch (ParseException e) {
            throw new ActionException(e);
          }
          pageState.addClientEntry( ceName, DateUtilities.dateToString(d));
        }
      } else {
        pageState.addClientEntry( ceName, value);
      }
    }
  }

  private void createClientEntryForDesignerField(DesignerData designerData, DesignerField df, String ceName, RestState restState)  {
    if (df != null) {
      String value = designerData.getValueMap().get(ceName);

      if (value == null && df.initialValue != null)
        value = df.initialValue;

      value=df.getValueForInputField(value);
      restState.addClientEntry(ceName, value);

    }
  }


}
