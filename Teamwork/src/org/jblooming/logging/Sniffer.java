package org.jblooming.logging;

import org.hibernate.CallbackException;
import org.hibernate.EmptyInterceptor;
import org.hibernate.Transaction;
import org.hibernate.engine.spi.SessionFactoryImplementor;
import org.hibernate.type.*;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.operator.Operator;
import org.jblooming.messaging.SomethingHappened;
import org.jblooming.ontology.Identifiable;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.persistence.ThreadLocalPersistenceContextCarrier;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.DateUtilities;

import java.io.Serializable;
import java.util.*;
import java.lang.reflect.Field;

/**
 * Audits field by field changes of persistent objects implementing the Auditable interface.
 * <p/>
 * Our aim is to log the transitive closure of changes, meaning that given an Auditable object, if this
 * has a many-to-one reference to another object, even if this last is not auditable, its changes must be recorded.
 * Same goes for collections of persistent entities. All this should end in a single record, describing in a
 * readable way all changes. Notice that if your Auditable object has a reference to an Auditable object, this
 * is treated simply, as you have to manage only the reference; but in all other cases, it's more complex.
 * <p/>
 * With this aim in mind, there are a few subtle points:
 * <p/>
 * - save of a new object: in order to cover all id generation cases, you have to be careful in particular for
 * native types, where in the onSave callback your object does not yet have an assigned id
 * <p/>
 * - update of an object: here you always need the "old" state of the object
 * <p/>
 * - delete of an object: you have to be careful of not creating a reference to the object which is being deleted,
 * as it may get in the way of hibernate deletion process
 * <p/>
 * - we want our analysis to be so fine grained, that if the same object is created, stored, modified, and then modified again
 * (and maybe even deleted :-) ) in the same session/transaction, we want to keep track of all phase changes, still in a single
 * record. In the code below there is this change matrix, but actually in the string output we have just the initial value and the
 * final one, but it can be easily empowered.
 * <p/>
 * Given these caveats, we have to build a "serialized" representation of all objects involved, and only at "post flush" we can
 * be sure that all data has been collected (even the new native ids), and hence the audit log record can be created (in a separate
 * session).
 *
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class Sniffer extends EmptyInterceptor {

  private static Set<InauditableClassProperties> inauditableClassProperties = new HashSet<InauditableClassProperties>();

  boolean log = false;

  enum ActionType {
    INSERT, PREPARE_UPDATE, UPDATE, DELETE
  }

  private List<History> histories = new ArrayList<History>();


  public boolean onLoad(
          Object entity,
          Serializable id,
          Object[] state,
          String[] propertyNames,
          Type[] types) {
    if (log)
      Tracer.platformLogger.info("AuditLogInterceptor.onLoad");

    return false;
  }


  public boolean onSave(Object entity, Serializable id, Object[] state, String[] propertyNames, Type[] types) throws CallbackException {

    if (log)
      Tracer.platformLogger.info("AuditLogInterceptor.onSave : " + entity.getClass() + ":" + id);

    History h = new History();

    h.actionType = ActionType.INSERT;
    Identifiable identifiable = (Identifiable) entity;
    h.theObjectTmp = identifiable;
    h.toBeRefreshed = true;
    h.theClass = identifiable.getClass();

    h.makeHistory(state, propertyNames, types);

    histories.add(h);
    return false;
  }

  public void onDelete(Object entity, Serializable serializable, Object[] state, String[] propertyNames, Type[] types) throws CallbackException {

    if (log)
      Tracer.platformLogger.info("AuditLogInterceptor.onDelete : " + entity.getClass() + ":" + ((Identifiable) entity).getId());

    Identifiable identifiable = (Identifiable) entity;

    History h = new History();
    h.actionType = ActionType.DELETE;
    h.theClass = identifiable.getClass();
    h.theId = identifiable.getId();

    //in order to manage cascade, we have to fill state from the object; state as given by hibernate is poor
    Object[] handReadState = new Object[propertyNames.length];
    for (int i = 0; i < propertyNames.length; i++) {
      //they should be added only if cascade delete on the collection
      try {
        handReadState[i] = ReflectionUtilities.getFieldValue(propertyNames[i], entity);
      } catch (Exception e) {
        throw new PlatformRuntimeException(e);
      }
    }

    h.makeHistory(handReadState, propertyNames, types);
    histories.add(h);
  }

  public boolean onFlushDirty(Object entity, Serializable id, Object[] newValues, Object[] oldValues, String[] properties, Type[] types)
          throws CallbackException {

    if (log)
      Tracer.platformLogger.info("AuditLogInterceptor.onFlushDirty : " + entity.getClass() + ":" + id);

    History oldHistory = new History();
    oldHistory.actionType = ActionType.PREPARE_UPDATE;
    Identifiable identifiable = (Identifiable) entity;
    oldHistory.theClass = identifiable.getClass();
    oldHistory.theId = identifiable.getId();
    oldHistory.makeHistory(oldValues, properties, types);
    histories.add(oldHistory);

    History newHistory = new History();
    newHistory.actionType = ActionType.UPDATE;
    newHistory.theClass = identifiable.getClass();
    newHistory.theId = identifiable.getId();
    newHistory.makeHistory(newValues, properties, types);
    histories.add(newHistory);

    return false;
  }


  public void preFlush(Iterator entities) {
    if (log)
      Tracer.platformLogger.info("AuditLogInterceptor.preFlush");

  }


  public void postFlush(Iterator iterator) throws CallbackException {

    if (log)
      Tracer.platformLogger.info("AuditLogInterceptor.postFlush");

    for (History h : histories) {
      if (h.toBeRefreshed) {
        h.refresh();
      }
    }

    Map<String, TraceRecord> trace = new TreeMap<String, TraceRecord>();

    buildTraceRecord(trace);
    /*for (History h : histories) {
      if (ReflectionUtilities.extendsOrImplements(h.theClass, Sniffable.class)) {
        trace.put(h.getKey(), updateTraceRecord(h, trace));
      }
    }*/

    for (String key : trace.keySet()) {

      TraceRecord tr = trace.get(key);
      if (!tr.isDependent) {
        AuditLogRecord entry = new AuditLogRecord();
        Operator operator = null;
        ThreadLocalPersistenceContextCarrier localPersistenceContextCarrier = PersistenceContext.threadLocalPersistenceContextCarrier.get();
        if (localPersistenceContextCarrier !=null)
          operator = localPersistenceContextCarrier.getOperator();
        entry.setFullName(operator != null ? operator.getFullname() : "no_logged_operator");
        entry.setCreated(new Date());
        entry.setMessage(tr.actionTypes.toString());
        entry.setEntityId(tr.theId.toString());
        entry.setEntityClass(tr.theClass.getName());
        entry.setData(tr.toStringBuffer(0).toString());

        if (tr.isAuditable) {
          AuditLogHelper.logEvent(entry);
        } else if (ReflectionUtilities.extendsOrImplements(tr.theClass, Sniffable.class)) {
          SomethingHappened sh = new SomethingHappened();
          // ...
        }
      }
    }
    histories.clear();
  }



  public int[] findDirty(Object entity,
                         Serializable id,
                         Object[] currentState,
                         Object[] previousState,
                         String[] propertyNames,
                         Type[] types) {
    if (log)
      Tracer.platformLogger.info("AuditLogInterceptor.findDirty");

    return null;
  }

  public String getEntityName(Object object) {
    return null;
  }

  public Object getEntity(String entityName, Serializable id) {
    return null;
  }

  public void afterTransactionBegin(Transaction tx) {}
  public void afterTransactionCompletion(Transaction tx) {}
  public void beforeTransactionCompletion(Transaction tx) {}

  public String onPrepareStatement(String sql) {
    return sql;
  }

  public void onCollectionRemove(Object collection, Serializable key) throws CallbackException {
    if (log)
      Tracer.platformLogger.info("AuditLogInterceptor.onCollectionRemove");
  }

  public void onCollectionRecreate(Object collection, Serializable key) throws CallbackException {
    if (log)
      Tracer.platformLogger.info("AuditLogInterceptor.onCollectionRecreate");

  }

  public void onCollectionUpdate(Object collection, Serializable key) throws CallbackException {
    if (log)
      Tracer.platformLogger.info("AuditLogInterceptor.onCollectionUpdate");

  }




  private void buildTraceRecord(Map<String, TraceRecord> trace) {

    for (History h : histories) {

      TraceRecord traceRecord = null;
      String key = h.getKey();

      //did I already visit this auditable ?
      if (trace.keySet().contains(key)) {
        traceRecord = trace.get(key);
        if (!ActionType.PREPARE_UPDATE.equals(h.actionType)) {
          traceRecord.actionTypes.add(h.actionType);
        }
      } else {
        traceRecord = new TraceRecord(h);
        if (!ActionType.PREPARE_UPDATE.equals(h.actionType))
          trace.put(key, traceRecord);
      }

      for (HistoryValue hv : h.values) {

        AuditRecord formerAuditRecord = traceRecord.propAuditRecord.get(hv.property);

        AuditRecord auditRecord = updateAuditRecord(h.actionType, hv, formerAuditRecord);
        if (auditRecord != null)
          traceRecord.propAuditRecord.put(hv.property, auditRecord);
      }
      trace.put(h.getKey(), traceRecord);
    }

    Set<String> visitedEntities = new HashSet();

    for (History h : histories) {
      if (!visitedEntities.contains(h.getKey())) {
        visitedEntities.add(h.getKey());
        TraceRecord traceRecord = trace.get(h.getKey());
        if (traceRecord != null && traceRecord.isAuditable) {
          for (AuditRecord ar : traceRecord.propAuditRecord.values()) {
            if (!ar.isPrimitive) {


              String traceRecordDependantKey = ar.newValue != null ? ar.newValue : ar.oldValue;
              if (traceRecordDependantKey != null) {

                TraceRecord traceRecordDependant = trace.get(traceRecordDependantKey);
                if (traceRecordDependant != null) {
                  ar.reference = traceRecordDependant;
                  traceRecordDependant.isDependent = true;
                }
              }
            }
          }
        }
      }
    }
  }

  private String prettyToString(Object o) {
    String s = "";
    if (o != null) {
      if (o instanceof Identifiable) {
        s = s + ReflectionUtilities.deProxy(o.getClass().getName())+((Identifiable)o).getId();
      } else if (o instanceof Date) {
        s = DateUtilities.dateAndHourToString((Date) o);
      } else
        s = "" + o;
    }
    return s;
  }

  private AuditRecord updateAuditRecord(ActionType currentAction, HistoryValue hv, AuditRecord auditRecord) {

    if (ActionType.INSERT.equals(currentAction) && hv.value != null) {

      //is it in arl ?

      if (auditRecord != null) {
        auditRecord.newValue = hv.value.toString();
      } else {
        auditRecord = new AuditRecord();
        auditRecord.property = hv.property;
        auditRecord.newValue = prettyToString(hv.value);
      }

    } else if (ActionType.PREPARE_UPDATE.equals(currentAction)) {

      //is it in arl ?

      if (auditRecord != null) {
        //do nothing
      } else {
        auditRecord = new AuditRecord();
        auditRecord.property = hv.property;
        auditRecord.oldValue = prettyToString(hv.value);
        auditRecord.newValue = null;
      }


    } else if (ActionType.UPDATE.equals(currentAction)) {

      //is it in arl ?

      if (auditRecord != null) {

        auditRecord.newValue = prettyToString(hv.value);

      } else {
        auditRecord = new AuditRecord();
        auditRecord.property = hv.property;
        auditRecord.newValue = prettyToString(hv.value);
      }

    } else if (ActionType.DELETE.equals(currentAction) && hv.value != null) {

      //is it in arl ?

      if (auditRecord != null) {
        auditRecord.oldValue = prettyToString(hv.value);
        auditRecord.newValue = null;

      } else {
        auditRecord = new AuditRecord();
        auditRecord.property = hv.property;
        auditRecord.oldValue = prettyToString(hv.value);
      }

    }

    if (auditRecord != null)
      auditRecord.isPrimitive = hv.isPrimitive;
    return auditRecord;
  }

  class TraceRecord {

    public Class<? extends Identifiable> theClass;
    public Serializable theId;
    public List<ActionType> actionTypes = new ArrayList<ActionType>();
    public Map<String, AuditRecord> propAuditRecord = new HashTable();
    public boolean isDependent = false;
    public boolean isAuditable = false;


    public TraceRecord(History h) {
      this.theClass = h.theClass;
      this.theId = h.theId;
      if (!ActionType.PREPARE_UPDATE.equals(h.actionType))
        actionTypes.add(h.actionType);
      isAuditable = ReflectionUtilities.extendsOrImplements(h.theClass, Auditable.class);
    }

    public StringBuffer toStringBuffer(int depth) {
      StringBuffer data = new StringBuffer();
      for (AuditRecord ar : propAuditRecord.values()) {
        data.append(StringUtilities.getRepeated(" ", depth)).append(ar.toStringBuffer(depth));
      }
      return data;
    }
  }

  class AuditRecord {

    public boolean isPrimitive;
    public String property;
    public String oldValue;
    public String newValue;
    public TraceRecord reference;
    public boolean relativeToAuditable = true;

    public StringBuffer toStringBuffer(int depth) {
      StringBuffer data = new StringBuffer();
      if (reference != null) {
        StringBuffer referenceMessage = reference.toStringBuffer(depth + 1);
        if (referenceMessage.length() > 0)
          data.append(property).append(":").append(reference.theClass.getName()).append(" ").append(reference.theId)
                  .append(":{<div class=\"audit\">").append(referenceMessage).append("</div>}\n");
      } else if ((oldValue != null && !oldValue.equals(newValue)) || (newValue != null && oldValue == null))
        data.append(property).append(":").append(oldValue).append("->").append(newValue).append('\n');
      return data;
    }

  }

  public class History {

    public Class<? extends Identifiable> theClass;
    public Serializable theId;
    public Identifiable theObjectTmp;
    boolean toBeRefreshed = false;
    public ActionType actionType;

    public List<HistoryValue> values = new ArrayList<HistoryValue>();

    public boolean equals(Object o) {
      return this.compareTo(o) == 0;
    }

    public int hashCode() {
      return (theClass.getName() + theId + actionType).hashCode();
    }

    public int compareTo(Object o) {
      History h = ((History) o);
      if (theClass == null || h == null || h.theClass == null)
        return -1;
      if (this == o)
        return 0;
      return toString().compareTo(h.toString());
    }

    public void makeHistory(Object[] states, String[] propertyNames, Type[] types) {

      boolean isInsert = ActionType.INSERT.equals(actionType);

      if (states != null && propertyNames != null && types != null) {
      for (int i = 0; i < types.length; i++) {

        Type type = types[i];
        Object value = states[i];
        String propertyName = propertyNames[i];

        makeHistoryElement(type, value, isInsert, propertyName, new HashSet());
      }
      }
    }

    private void makeHistoryElement(Type type, Object value, boolean insert, String propertyName, Collection visitedEntities) {

      if (!visitedEntities.contains(value)) {

        //verify that we are uditing
        if (!doAuditPropertyOfClass(theClass, propertyName)) {
          return;
        }

        visitedEntities.add(value);

        if (type instanceof ManyToOneType || type instanceof OneToOneType) {

          Identifiable ident = (Identifiable) value;
          if (ident != null) {
            ident = ReflectionUtilities.getUnderlyingObject(ident);
            if (insert)
              addValueTmp(type, propertyName, ident);
            else
              addValue(type, propertyName, ReflectionUtilities.deProxy(ident.getClass().getName()) + ident.getId(), false);
          }

        } else if (type instanceof MapType) {

          Type elementType = ((MapType) type).getElementType((SessionFactoryImplementor) HibernateFactory.getSessionFactory());
          if (value != null) {
            //todo: keys of maps must be primitive
            for (Object el : ((Map) value).keySet()) {
              makeHistoryElement(elementType, ((Map) value).get(el), insert, propertyName + "[" + el + "]", visitedEntities);
            }
          }

        } else if (type instanceof CollectionType) {

          Type elementType = ((CollectionType) type).getElementType((SessionFactoryImplementor) HibernateFactory.getSessionFactory());
          int i = 0;
          if (value != null) {
            for (Object el : (Collection) value) {
              makeHistoryElement(elementType, el, insert, propertyName + "[" + i + "]", visitedEntities);
              i++;
            }
          }

        } else

          addValue(type, propertyName, value, true);
      }
    }


    public void addValue(Type type, String property, Object value, boolean isPrimitive) {
      HistoryValue hv = new HistoryValue();
      hv.type = type;
      hv.property = property;
      hv.value = value;
      hv.isPrimitive = isPrimitive;
      values.add(hv);

    }

    public void addValueTmp(Type type, String property, Identifiable value) {
      HistoryValue hv = new HistoryValue();
      hv.type = type;
      hv.property = property;
      hv.valueTmp = value;
      hv.toBeRefreshed = true;
      hv.isPrimitive = false;
      values.add(hv);
    }


    public void refresh() {

      theId = theObjectTmp.getId();
      theObjectTmp = null;
      toBeRefreshed = false;
      for (HistoryValue hv : values) {
        hv.refresh();
      }
    }

    public String getKey() {
      return theClass.getName() + theId.toString();
    }

    public String toString() {

      String s = " actionType:" + actionType + " toBeRefreshed:" + toBeRefreshed;
      if (theClass != null)
        s = s + theClass.getName();
      if (theId != null)
        s = s + theId;

      for (HistoryValue hv : values) {
        s = s + hv.value + hv.valueTmp;
      }

      return s;

    }
  }

  public class HistoryValue {

    public Type type;
    public String property;
    public Identifiable valueTmp;
    public Object value;
    boolean toBeRefreshed = false;
    boolean isPrimitive = true;

    public void refresh() {
      if (toBeRefreshed) {
        value = ReflectionUtilities.deProxy(valueTmp.getClass().getName()) + valueTmp.getId();
        valueTmp = null;
        toBeRefreshed = false;
      }
    }

    public String getKey() {
      if (isPrimitive)
        throw new PlatformRuntimeException("HistoryValue.getKey on  primitive");
      return value != null ? value.toString() : null;
    }
  }

  public static void addInauditedPropertyOfClass(Class aClass, String property) {

    if (aClass != null && !ReflectionUtilities.extendsOrImplements(aClass, Sniffable.class))
      throw new PlatformRuntimeException("Only Auditable classes need to be hand excluded from audit");
    InauditableClassProperties icp = new InauditableClassProperties();
    icp.auditableClass = aClass;
    icp.property = property;
    inauditableClassProperties.add(icp);
  }

  public static void addInauditedProperty(String property) {
    addInauditedPropertyOfClass(null, property);
  }

  static class InauditableClassProperties {

    public Class auditableClass;
    public String property;
  }

  public static boolean doAuditPropertyOfClass(Class aClass, String property) {

    boolean result = true;

    Field field = ReflectionUtilities.getDeclaredInheritedFields(aClass).get(property);
    if (field != null)
      result = !field.isAnnotationPresent(Inaudited.class);

    if (result) {
      for (InauditableClassProperties classProperties : inauditableClassProperties) {
        if (
                (classProperties.auditableClass == null || classProperties.auditableClass.equals(aClass)) &&
                        classProperties.property.equals(property)) {
          result = false;
          break;
        }
      }
    }

    return result;
  }

}