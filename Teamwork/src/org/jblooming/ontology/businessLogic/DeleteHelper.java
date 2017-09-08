package org.jblooming.ontology.businessLogic;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;
import org.hibernate.HibernateException;
import org.hibernate.MappingException;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.hibernate.engine.CascadesProxy;
import org.hibernate.engine.spi.SessionFactoryImplementor;
import org.hibernate.internal.SessionImpl;
import org.hibernate.mapping.Column;
import org.hibernate.mapping.PersistentClass;
import org.hibernate.mapping.Property;
import org.hibernate.persister.entity.EntityPersister;
import org.hibernate.type.CollectionType;
import org.hibernate.type.EntityType;
import org.hibernate.type.OneToOneType;
import org.hibernate.type.Type;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.logging.DeleteLog;
import org.jblooming.messaging.Listener;
import org.jblooming.ontology.*;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.persistence.hibernate.HibernateUtilities;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.security.License;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.ObjectEditorConstants;
import org.jblooming.waf.view.RestState;

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.*;

public class DeleteHelper {


  public static void cmdDelete(IdentifiableSupport delendo, RestState pageState) throws PersistenceException {

    try {
      delendo = (IdentifiableSupport) ReflectionUtilities.getUnderlyingObject(delendo);
      boolean delendaIsNode = delendo instanceof Node;
      Node parent = null;
      if (delendaIsNode)
        parent = ((Node) delendo).getParentNode();

      Class objClass = delendo.getClass();
      String clazzName = ReflectionUtilities.deProxy(objClass.getName());
      objClass = Class.forName(clazzName);
      PersistenceContext persistenceContext = PersistenceContext.get(delendo);
      SessionFactory sf = persistenceContext.persistenceConfiguration.getSessionFactory();

      EntityPersister entityPersister = (EntityPersister) sf.getClassMetadata(objClass);
      PersistentClass pclass = HibernateUtilities.getClassMapping(objClass);

      List<Property> allProps = new ArrayList();

      //si prendono le proprietà mappate
      Iterator it = pclass.getPropertyClosureIterator();
      while (it.hasNext()) {
        allProps.add((Property) it.next());
      }
      CascadesProxy csp = new CascadesProxy();


      DeleteLog deleteLog = new DeleteLog();

      deleteLog.setEntityClass(clazzName);
      deleteLog.setEntityId(delendo.getId()+"");
      deleteLog.setEntityName(JSP.limWr(delendo.getDisplayName(),250));
      deleteLog.setDeletedOn(new Date());
      deleteLog.setDeletedBy(JSP.w(pageState.getLoggedOperator() == null ? "--system--" : pageState.getLoggedOperator().getDisplayName()));

      JSONObject jsonDeleteLog = deleteLog.getJsonData();
      jsonDeleteLog.element(objClass.getSimpleName(), delendo.jsonify());

      for (Property property : allProps) {

        String name = property.getName();

        Object propertyValue = entityPersister.getPropertyValue(delendo, name);

        if (propertyValue != null) {

          boolean doesPropertyCascade = csp.doesCascadeOnDelete(property.getCascadeStyle());

          // è una collezione non vuota?
          if (propertyValue instanceof Collection && ((Collection) propertyValue).size() > 0) {
            Collection coll = (Collection) propertyValue;
            Object sample = coll.iterator().next();
            boolean membersAreChildren = delendaIsNode && coll.equals(((Node) delendo).getChildrenNode());

            // abbiamo una collezione non vuota di oggetti identifiable
            if (sample instanceof Identifiable) {

              String childDelStyle = pageState.getEntry(ObjectEditorConstants.FLD_DELETE_STYLE + "__" + name).stringValueNullIfEmpty();
              boolean globalUnlinkForThisColl = Commands.UNLINK.equals(childDelStyle);
              boolean globalUpForThisColl = Commands.UP.equals(childDelStyle);
              boolean globalDelDescForThisColl = Commands.DELETE_DESCENDANTS.equals(childDelStyle);


              JSONArray jsColl = new JSONArray();

              //for (Iterator iterator = new ArrayList(coll).iterator(); iterator.hasNext();) {
              for (Object o : coll) {
                IdentifiableSupport memberOfCollection = (IdentifiableSupport) o;

                //se la collezione fa il delete cascade devo loggare
                if (doesPropertyCascade) {
                  jsColl.add(memberOfCollection.jsonify());


                  // move to root
                } else if (globalUnlinkForThisColl) {

//                    //remove from this collection if is no cascade
//                    if (!doesPropertyCascade)
//                      coll.remove(memberOfCollection);

                  if (membersAreChildren) {
                    if (memberOfCollection instanceof PerformantNode)
                      ((PerformantNodeSupport) memberOfCollection).setParentAndStore(null);
                    else
                      ((Node) memberOfCollection).setParentNode(null);
                  }

                  //move to parent
                } else if (globalUpForThisColl) {

//                    //remove from this collection if is no cascade
//                    if (!doesPropertyCascade)
//                      coll.remove(memberOfCollection);

                  if (membersAreChildren) {
                    if (memberOfCollection instanceof PerformantNode)
                      ((PerformantNodeSupport) memberOfCollection).setParentAndStore((PerformantNodeSupport) parent);
                    else {
                      ((Node) memberOfCollection).setParentNode(parent);
                      //add to parent's collection
                      ((Collection) entityPersister.getPropertyValue(parent, name)).add(memberOfCollection);
                    }
                  } else {
                    //add to parent's collection
                    ((Collection) entityPersister.getPropertyValue(parent, name)).add(memberOfCollection);
                  }

                  //delete also members
                } else if (globalDelDescForThisColl) {

//                    //remove from this collection if is no cascade
//                    if (!doesPropertyCascade)
//                      coll.remove(memberOfCollection);

                  Node node = (Node) memberOfCollection;
                  if (membersAreChildren) {
                    node.setParentNode(null);
                    //recursivelyDeleteNode(node, pageState);
                    cmdDelete(memberOfCollection, pageState);
                  } else {
                    jsColl.element(memberOfCollection.jsonify());
                    deleteIdentifiable(memberOfCollection, pageState);
                  }
                }
              }

              if (jsColl.size() > 0) {
                jsonDeleteLog.element(name, jsColl);
              }

            }

          //non è una collezione piena
          } else if(propertyValue instanceof Identifiable) {
            IdentifiableSupport prop = (IdentifiableSupport) propertyValue;
            if (doesPropertyCascade) {
              jsonDeleteLog.element(name, prop.jsonify());
            }
          }
        }
      }


      //se il delendo è un node deve essere rimosso dalla lista dei figli del parent, perchè se il parent è già stato
      if (delendo instanceof PerformantNode){
        PerformantNode pdd= (PerformantNode) delendo;
        //remove from old parent children
        if(pdd.getParentNode() != null ){
          pdd.getParentNode().getChildrenNode().remove(pdd);
        }
      }

      deleteIdentifiable(delendo, pageState);

      if (License.assertLevel(30))
        deleteLog.store();

    } catch (ClassNotFoundException e) {
      throw new PlatformRuntimeException(e);
    } catch (HibernateException e) {
      throw new PlatformRuntimeException(e);
    }
  }

  private static void deleteIdentifiable(IdentifiableSupport ident, RestState ps) throws RemoveException {

    PersistenceContext dpc = PersistenceContext.get(ident);

    try {
      ident.remove();
      dpc.session.flush();
      // delete all relative listeners
      QueryHelper qh = new QueryHelper("from " + Listener.class.getName());
      qh.addQBEClause("theClass", "theClass", ident.getClass().getName(), QueryHelper.TYPE_CHAR);
      qh.addQBEClause("identifiableId", "identifiableId", ident.getId().toString(), QueryHelper.TYPE_CHAR);
    } catch (Throwable e) {

      Transaction t = dpc.session.getTransaction();
      if (t != null && !t.wasRolledBack()) {
        t.rollback();
        dpc.session.beginTransaction();
      }
      Serializable id = ident.getId();
      Class cl = ReflectionUtilities.getUnderlyingObjectClass(ident);
      // 3Dec2007 changed to clear: evict could leave cascading refences "alive" and set as deleted
      //dpc.session.evict(ident);
      dpc.session.clear();

      try {
        ident = (IdentifiableSupport) PersistenceHome.findByPrimaryKey(cl, id);
      } catch (FindByPrimaryKeyException e1) {
      }

      ps.setMainObject(ident);
      ps.resetLoggedOperator();

      throw new RemoveException(e);
    }
  }

  public static void recursivelyDeleteNode(Node node, RestState ps) throws RemoveException {

    Collection children = node.getChildrenNode();
    if (children != null && children.size() > 0) {
      for (Iterator iterator = children.iterator(); iterator.hasNext(); ) {
        Node child = (Node) iterator.next();
        recursivelyDeleteNode(child, ps);
      }
    }
    deleteIdentifiable((IdentifiableSupport) node, ps);
  }

  public static void cmdDisintegrate(Collection disintegrandas) throws PersistenceException {
    for (Object r : disintegrandas) {
      DeleteHelper.cmdDisintegrate((IdentifiableSupport) r);
    }
  }

  public static void cmdDisintegrate(IdentifiableSupport delendo) throws PersistenceException {

    PersistenceContext pc = new PersistenceContext();
    Object underlyingObject = ReflectionUtilities.getUnderlyingObjectAsObject(delendo);
    Object value = null;
    try {
      value = underlyingObject.getClass().getMethod("getId").invoke(underlyingObject);
    } catch (Exception e) {
      throw new PlatformRuntimeException(e);
    }

    Object realDelendo = null;
    try {
      realDelendo = PersistenceHome.findUniqueObject(underlyingObject.getClass(), "id", value, pc);
    } finally {
      pc.commitAndClose();
    }

    pc = new PersistenceContext();

    Class realClass = realDelendo.getClass();

    Map<String, org.hibernate.mapping.Collection> inverses = HibernateUtilities.getAllInversesOnTarget(realDelendo);
    PersistenceContext persistenceContext = PersistenceContext.get(delendo);
    SessionFactory sf = persistenceContext.persistenceConfiguration.getSessionFactory();

    PersistentClass realClassPc = HibernateUtilities.getClassMapping(realClass);

    CascadesProxy csp = new CascadesProxy();

    Iterator i = persistenceContext.persistenceConfiguration.getHibernateConfiguration().getClassMappings();
    while (i.hasNext()) {

      PersistentClass persistentClass = (PersistentClass) i.next();
      Iterator j = persistentClass.getPropertyClosureIterator();
      while (j.hasNext()) {

        Property property = (Property) j.next();
        Column col = null;
        if (property.getColumnIterator().hasNext())
          col = (Column) property.getColumnIterator().next();

        //is there a cascading inverse for this property on realClass ?
        boolean cascadingInverseOnRealClass = false;

        for (String key : inverses.keySet()) {

          org.hibernate.mapping.Collection hibInvCollOfDelendo = inverses.get(key);
          try {
            Property propertyWhichIsCollection = realClassPc.getProperty(key);
            if (((Column) (hibInvCollOfDelendo.getKey().getColumnIterator().next())).getName().equals(property.getName()) &&
              csp.doesCascadeOnDelete(propertyWhichIsCollection.getCascadeStyle())
              ) {
              cascadingInverseOnRealClass = true;
              break;
            }
          } catch (MappingException e) {
          }
        }

        boolean isNullable = property.getValue().isNullable();

        //remove references from entities to delendo
        if (
          !cascadingInverseOnRealClass &&
            isNullable &&
            property.getType() instanceof EntityType &&
            !(property.getType() instanceof OneToOneType) &&
            property.getType().getReturnedClass() != null &&
            //property.getType().getReturnedClass().getName().equals(realClass.getName()) &&
            realClassPc.getTable().getName().equals(HibernateUtilities.getTableName(property.getType().getReturnedClass())) &&
            //ReflectionUtilities.extendsOrImplements(realClass, property.getType().getReturnedClass()) &&
            !csp.doesCascadeOnDelete(property.getCascadeStyle())
          ) {
          String hql = "update " + persistentClass.getEntityName() + " set " + property.getName() + "=null where " + property.getName() + " = :disintegrando";
          OqlQuery oql = new OqlQuery(hql, pc);
          oql.getQuery().setEntity("disintegrando", realDelendo);
          Tracer.platformLogger.debug("cmdDisintegrate reference " + hql);
          oql.getQuery().executeUpdate();

          //remove references to delendo from non inverse external collections
        } else if (
          property.getType() instanceof CollectionType
          ) {

          Type elementType = ((CollectionType) property.getType()).getElementType((SessionFactoryImplementor) sf);
          if (elementType.isEntityType()) {
            Class collectionOf = elementType.getReturnedClass();
            String tableName = HibernateUtilities.getTableName(collectionOf);
            if (tableName != null && realClassPc.getTable().getName().equals(tableName)) {
              org.hibernate.mapping.Collection collection = persistenceContext.persistenceConfiguration.getHibernateConfiguration().getCollectionMapping(((CollectionType) property.getType()).getRole());
              if (!collection.isInverse()) {
                String valueColumn = ((Column) collection.getElement().getColumnIterator().next()).getName();
                PreparedStatement ps = null;
                try {
                  String sql = "DELETE FROM " + collection.getCollectionTable().getName() + " WHERE " + valueColumn + "= ?";
                  ps = ((SessionImpl) pc.session).connection().prepareStatement(sql);
                  Tracer.platformLogger.debug("cmdDisintegrate CollectionType " + sql);
                  //ps.setString(1, realDelendo.getId().toString());
                  ps.setString(1, realDelendo.getClass().getMethod("getId").invoke(realDelendo).toString());
                  ps.executeUpdate();
                  ps.close();
                } catch (SQLException e) {
                  throw new PlatformRuntimeException(e);
                } catch (NoSuchMethodException e) {
                  throw new PlatformRuntimeException(e);
                } catch (IllegalAccessException e) {
                  throw new PlatformRuntimeException(e);
                } catch (InvocationTargetException e) {
                  throw new PlatformRuntimeException(e);
                }
              }
            }
          }
        }
      }
    }

    pc.session.delete(realDelendo);
    pc.commitAndClose();
  }


}

//}
