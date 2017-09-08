package com.twproject.rank;

import com.twproject.resource.Resource;
import com.twproject.operator.TeamworkOperator;
import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;
import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.persistence.PersistenceBricks;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.persistence.exceptions.*;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.CodeValue;
import org.jblooming.utilities.CodeValueList;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.oql.OqlQuery;
import org.jblooming.security.SecurableWithArea;
import org.jblooming.security.Area;
import org.jblooming.tracer.Tracer;

import javax.persistence.*;
import java.util.*;
import java.io.Serializable;

@Entity
@Table(name = "twk_rank_hit")
public class Hit extends IdentifiableSupport {

  private String entityClass;
  private String entityId;
  private int operatorId;
  private double weight;
  private long when;
  private int areaId;

  public static final double WEIGHT_MIN = 0.1;
  public static final double WEIGHT_MAX = 1;

  /**
   * Edit operation should be below .5, and save from .5 upwards.
   *
   * @return
   */

  @Id
  @Type(type = "int")
  @GeneratedValue(strategy = GenerationType.AUTO)
  public Serializable getId() {
    return super.getId();
  }


  @Index(name = "idx_hit_op")
  public int getOperatorId() {
    return operatorId;
  }

  public void setOperatorId(int operatorId) {
    this.operatorId = operatorId;
  }

  public double getWeight() {
    return weight;
  }

  public void setWeight(double weight) {
    //if (weight<WEIGHT_MIN || weight>WEIGHT_MAX)
    //  throw new PlatformRuntimeException("weight should be between" + WEIGHT_MIN+ " and "+WEIGHT_MAX);

    if (weight < WEIGHT_MIN)
      weight = WEIGHT_MIN;
    else if (weight > WEIGHT_MAX)
      weight = WEIGHT_MAX;

    this.weight = weight;
  }

  @Column(name = "whenx")
  @Index(name = "idx_hit_when")
  public long getWhen() {
    return when;
  }

  public void setWhen(long when) {
    this.when = when;
  }


  @Index(name = "idx_hit_entClass")
  public String getEntityClass() {
    return entityClass;
  }

  public void setEntityClass(String entityClass) {
    this.entityClass = entityClass;
  }

  @Index(name = "idx_hit_entId")
  public String getEntityId() {
    return entityId;
  }

  public void setEntityId(String entityId) {
    this.entityId = entityId;
  }


  private static Hit newHit(Identifiable i, TeamworkOperator logged, double weight) {
    Hit hit = new Hit();
    hit.setEntityClass(ReflectionUtilities.deProxy(i.getClass().getName()));
    hit.setEntityId(i.getId() + "");
    hit.setOperatorId((Integer) logged.getId());
    hit.setWhen(System.currentTimeMillis());
    hit.setWeight(weight);
    if (i instanceof SecurableWithArea) {
      Area area = ((SecurableWithArea) i).getArea();
      if (area!=null)
        hit.setAreaId((Integer) area.getId());
    } 

    if (hit.getAreaId()==0 && logged.getPerson()!=null && logged.getPerson().getArea()!=null)
      hit.setAreaId((Integer)logged.getPerson().getArea().getId());
    return hit;
  }


  public static Hit getInstanceAndStore(Identifiable i, TeamworkOperator logged, double weight) throws StoreException {
    Hit hit = newHit(i, logged, weight);
    hit.store();
    return hit;
  }

  public static Set<Hit> getInstanceAndStore(Identifiable i, Collection<Resource> involved, double weight) throws StoreException {
    Set hits = new HashSet();
    for (Resource person : involved) {
      if (person.getMyself() != null) {
        hits.add(getInstanceAndStore(i, person.getMyself(), weight));
      }
    }
    return hits;
  }

  public String toString() {
    return entityClass + " id: " + entityId + " op id:" + operatorId + " weight:" + weight + " when:" + JSP.w(new Date(when));
  }

  public static void removeDeleted(Date since) throws PersistenceException {

      CodeValueList valueList = PersistenceBricks.getPersistentEntities(IdentifiableSupport.class);
      valueList.sort();

      for (CodeValue codeValue : valueList.getList()) {

        String entityClass = codeValue.code;

        //get hitted ids
        String hql = "select distinct hit.entityId from " + Hit.class.getName() + " as hit where hit.entityClass = :entityClass";
        if (since!=null)
          hql = hql + " and hit.when > :when";
          OqlQuery oql = new OqlQuery(hql);
        oql.getQuery().setString("entityClass", entityClass);
        if (since!=null)
          oql.getQuery().setLong("when", since.getTime());

        List<String> ids = oql.list();

        if (JSP.ex(ids)) {
          //get entities hitted
          //no good: hsqldb does not have trim command
          //sqlSelect = "select trim(str(hitted.id)) from " + entityClass + " as hitted";
          //sqlSelect = "select str(hitted.id) from " + entityClass + " as hitted";
          hql = "select hitted.id from " + entityClass + " as hitted";

          oql = new OqlQuery(hql);
          List existingHittedIdsWithSpaces = oql.list();
          List<String> existingHittedIds = new ArrayList();
          for (Object idWithSpaces : existingHittedIdsWithSpaces) {
            existingHittedIds.add((idWithSpaces+"").trim());
          }
          
          ids.removeAll(existingHittedIds);

          for (String presumedEntityHittedId : ids) {

            Tracer.platformLogger.info("Hit repairing: not found "+entityClass+" of id "+presumedEntityHittedId);

            //delete all such hits
            //sqlSelect = "select hit from " + Hit.class.getName() + " as hit where hit.entityClass = :entityClass and hit.entityId=:entityId";
            hql = "delete from " + Hit.class.getName() + " where entityClass = :entityClass and entityId=:entityId";
            oql = new OqlQuery(hql);
            oql.getQuery().setString("entityClass", entityClass);
            oql.getQuery().setString("entityId", presumedEntityHittedId);
            /*List<Hit> damHits = oql.list();
            int i = 0;
            for (Hit damHit : damHits) {
              i++;
              damHit.remove();
              if (i % 20 == 0) {
                PersistenceContext pc = PersistenceContext.getDefaultPersistenceContext();
                pc.session.flush();
                pc.session.clear();
              }
            }*/
            oql.getQuery().executeUpdate();
            PersistenceContext pc = PersistenceContext.getDefaultPersistenceContext();
            pc.session.flush();
            pc.session.clear();
          }
        }
      }

  }


  public int getAreaId() {
    return areaId;
  }

  public void setAreaId(int areaId) {
    this.areaId = areaId;
  }
}
