package org.jblooming.flowork;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.ontology.LoggableIdentifiableSupport;
import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.StringUtilities;

import java.lang.reflect.Field;
import java.util.*;

public class FlowFields extends LoggableIdentifiableSupport {

  private String flowName;
  private String flowVersion;

  private Map<String,String> nodeDescriptions = new HashTable<String,String>();

  /**
   * this map use the token name as key and a "serialized" fields collection
   */
  private Map<String,String> nodeFields = new HashTable<String,String>();

  public static final String MANDATORY_FIELD = "MAND_FLD";
  public static final String READ_ONLY_FIELD = "R_O_FLD";
  public static final String FLOWFIELDS = "FLOWFIELDS";


  public String getFlowName() {
    return flowName;
  }

  public void setFlowName(String flowName) {
    this.flowName = flowName;
  }

  public String getFlowVersion() {
    return flowVersion;
  }

  public void setFlowVersion(String flowVersion) {
    this.flowVersion = flowVersion;
  }

  public void putNodeFields(String key, String value) {
    nodeFields.put(key, value);
  }

  public void removeTokenFieldsByKey(String key) {
    nodeFields.remove(key);
  }

  public void removeTokenFieldsByValue(String value) {
    Iterator keySet = new HashSet(nodeFields.keySet()).iterator();
    while (keySet.hasNext()) {
      final String key = (String) keySet.next();
      String loopValue = getTokenFields(key);
      if (loopValue.equals(value)) {
        removeTokenFieldsByKey(key);
      }
    }
  }

  public String getTokenFields(String key) {
    return (String) nodeFields.get(key);
  }

  public Iterator getTokenFieldsKeysetIterator() {
    return nodeFields.keySet().iterator();
  }

  public int TokenFieldsKeysetSize() {
    return nodeFields.keySet().size();
  }

  public List<FormFieldConfiguration> getFormFields(String nodeName) {
    List formFields = new ArrayList();
    String serVal = getTokenFields(nodeName);
    if (serVal != null) {
      List fields = StringUtilities.splitToList(serVal, ",");
      if (fields != null && fields.size() > 0) {
        for (Iterator iterator = fields.iterator(); iterator.hasNext();) {

          String fieldSer = (String) iterator.next();
          List fieldAttr = StringUtilities.splitToList(fieldSer, "__+__");
          String fldName = fieldSer.substring(0, fieldSer.indexOf("__+__"));
          FormFieldConfiguration ffld = new FormFieldConfiguration(fldName);
          ffld.required = fieldAttr.contains(MANDATORY_FIELD);
          ffld.readOnly = fieldAttr.contains(READ_ONLY_FIELD);
          formFields.add(ffld);
        }
      }
    }
    Collections.sort(formFields, new FormFieldComparator());
    return formFields;
  }

  public List<FormFieldConfiguration> getFormFields() {
    List formFields = new ArrayList();
    if (nodeFields != null && nodeFields.size() > 0) {
      for (Object value : nodeFields.values()) {
        String serVal = (String) value;
        if (serVal != null) {
          List fields = StringUtilities.splitToList(serVal, ",");
          if (fields != null && fields.size() > 0) {
            for (Iterator iterator = fields.iterator(); iterator.hasNext();) {
              String fieldSer = (String) iterator.next();
              List fieldAttr = StringUtilities.splitToList(fieldSer, "__+__");
              String fldName = fieldSer.substring(0, fieldSer.indexOf("__+__"));
              FlowFields.FormFieldConfiguration ffld = new FlowFields.FormFieldConfiguration(fldName);
              ffld.required = fieldAttr.contains(FlowFields.MANDATORY_FIELD);
              ffld.readOnly = fieldAttr.contains(FlowFields.READ_ONLY_FIELD);
              formFields.add(ffld);
            }
          }
        }
      }
    }
    Collections.sort(formFields, new FormFieldComparator());
    return formFields;
  }



  public List getAllFormFields() {
    List formFields = new ArrayList();
    Iterator nodes = getTokenFieldsKeysetIterator();
    Set propNames = new HashSet();
    while (nodes.hasNext()) {
      String nodeName = (String) nodes.next();
      List potFF = getFormFields(nodeName);
      for (int i = 0; i < potFF.size(); i++) {
        FormFieldConfiguration formFieldConfiguration = (FormFieldConfiguration) potFF.get(i);
        if (!propNames.contains(formFieldConfiguration.id)) {
          formFields.add(formFieldConfiguration);
          propNames.add(formFieldConfiguration.id);
        }
      }

    }
    Collections.sort(formFields, new FormFieldComparator());

    return formFields;
  }

  public Map getNodeFields() {
    return nodeFields;
  }

  public void setNodeFields(Map nodeFields) {
    this.nodeFields = nodeFields;
  }

  public Map<String,String> getNodeDescriptions() {
    return nodeDescriptions;
  }

  public void setNodeDescriptions(Map<String,String> nodeDescriptions) {
    this.nodeDescriptions = nodeDescriptions;
  }

  public static int definitionStepFieldNumber(String stepName, String definitionName) throws PersistenceException {
    FlowFields ff = (FlowFields) PersistenceHome.findUnique(FlowFields.class, "flowName", definitionName);
    if (ff != null)
      return ff.getFormFields(stepName).size();
    else
      return 0;


  }


  class FormFieldComparator implements Comparator {
    public int compare(Object b, Object a) {
      return ((FormFieldConfiguration) b).id.compareToIgnoreCase(((FormFieldConfiguration) a).id);
    }
  }



  /**
   *
   * @param nodeName
   * @param form
   * @return true if the required fields of "form" for the "nodeName" step are fullfilled 
   */

  public boolean requiredFieldFilled(String nodeName,Object form){
    boolean filled=true;
    List fields = getFormFields(nodeName);
    for (Iterator iterator = fields.iterator(); iterator.hasNext() && filled;) {
      FormFieldConfiguration fieldConfiguration =  (FormFieldConfiguration) iterator.next();
      if (fieldConfiguration.required){
        final Field reflectionField;
        try {
          reflectionField = form.getClass().getDeclaredField(fieldConfiguration.id);
          reflectionField.setAccessible(true);
        } catch (NoSuchFieldException e) {
          throw new PlatformRuntimeException(e);
        }
        Object fieldValue ;
        try {
          fieldValue = reflectionField.get(form);
        } catch (IllegalAccessException e) {
          throw new PlatformRuntimeException(e);
        }
        if (fieldValue==null){
          filled=false;
        }
      }
    }
    return filled;
  }



  public class FormFieldConfiguration {

    public String id;
    public boolean required = false;
    public boolean readOnly = false;

    public FormFieldConfiguration(String name) {
      this.id = name;

    }

  }



}
