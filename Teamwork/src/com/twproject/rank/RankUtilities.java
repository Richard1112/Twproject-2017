package com.twproject.rank;

import com.twproject.resource.Resource;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.utilities.JSP;

import java.util.*;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Jan 25, 2008
 * Time: 11:43:40 AM
 */
public class RankUtilities {

  public static List<Hit> getRecentHitsOfOperator(int operatorId, Date date) throws FindException {
    return getRecentHitsOfOperator(operatorId, null,date);
  }

  public static List<Hit> getRecentHitsOfOperator(int operatorId, String className, Date date) throws FindException {

    String hql = "from " + Hit.class.getName() + " as hit where hit.when between :whenx1 and :whenx2 and hit.operatorId=:operatorId";
    if (JSP.ex(className))
        hql=hql+" and hit.entityClass=:entCl";
    hql=hql+" order by hit.when desc";
    //String sqlSelect = "from " + Hit.class.getName() + " as hit where hit.operatorId=:operatorId";

    OqlQuery oql = new OqlQuery(hql);
    //oql.getQuery().setMaxResults(100);
    oql.getQuery().setLong("whenx2", date.getTime());
    oql.getQuery().setLong("whenx1", date.getTime()-CompanyCalendar.MILLIS_IN_MONTH);
    oql.getQuery().setInteger("operatorId", operatorId);

    if (JSP.ex(className))
      oql.getQuery().setString("entCl", className);

    List<Hit> hits = oql.list();
    return hits;
  }

  //one could refine for particular operator
  public static double getWeightForEntity(IdentifiableSupport entity, Date date) throws FindException {

    String hql = "from " + Hit.class.getName() + " as hit where hit.when between :whenx1 and :whenx2 and hit.entityClass=:entityClass and hit.entityId = :entityId order by hit.when desc";
    //String sqlSelect = "from " + Hit.class.getName() + " as hit where hit.operatorId=:operatorId";

    OqlQuery oql = new OqlQuery(hql);
    //oql.getQuery().setMaxResults(100);
    CompanyCalendar cc = new CompanyCalendar(date);
    oql.getQuery().setLong("whenx2", cc.getTimeInMillis());
    cc.add(CompanyCalendar.MONTH, -1);
    oql.getQuery().setLong("whenx1", cc.getTimeInMillis());
    oql.getQuery().setString("entityClass", entity.getClass().getName());
    oql.getQuery().setString("entityId", entity.getId()+"");

    List<Hit> hits = oql.list();
    double result=0;
    if (hits.size()>0) {
      Map<String, Double> wfe = computeWeightForEntities(hits);
      result=wfe.values().iterator().next();
    }
    return result;
  }



  public static Map<String, Double> computeWeightForEntities(List<Hit> hits) {
    Map<String, Double> weights = new HashTable<String, Double>();
    for (Hit hit : hits) {
      Double w = 0d;
      if (weights.containsKey(hit.getEntityClass() + "^" + hit.getEntityId())) {
        w = weights.get(hit.getEntityClass() + "^" + hit.getEntityId());
      }
      double distInDays = (System.currentTimeMillis() - hit.getWhen()) / CompanyCalendar.MILLIS_IN_DAY;
      Double computed = hit.getWeight() * getGauss(distInDays);
      weights.put(hit.getEntityClass() + "^" + hit.getEntityId(), w + computed);
    }
    return weights;
  }


  public static double getGauss(double x) {
    double a = 1; // a>1 amplifies recent hits
    double c = 30; // c>1 wides the bell
    double b = 0; // b shift the bell on x axis
    return a * Math.exp(-(Math.pow(x - b, 2)) / (c));
  }

  public static List<EntityGroupRank> getRanked(Map<String, Double> weights, int maxSize) {
    List<EntityGroupRank> grr = new LinkedList<EntityGroupRank>();
    for (String classPlusId : weights.keySet()) {
      EntityGroupRank egr = new EntityGroupRank();
      //egr.classAndId = classPlusId;
      egr.className = classPlusId.split("\\^")[0];
      egr.id = classPlusId.split("\\^")[1];
      egr.weight = weights.get(classPlusId);
      grr.add(egr);
    }
    Collections.sort(grr);

    //hidrate
    int i = 0;
    List<EntityGroupRank> topMaxSize = new LinkedList<EntityGroupRank>();
    for (EntityGroupRank egr : grr) {
      Identifiable entity = egr.getEntity();
      if (entity != null) {
        topMaxSize.add(egr);
        i++;
      }
      if (i >= maxSize)
        break;
    }
    return topMaxSize;
  }

  public static List<EntityGroupRank> getEntityRankStatistically(int operatorId, String className,  Date date) throws PersistenceException {
    return getRanked(computeWeightForEntities(getRecentHitsOfOperator(operatorId,className,date)), 20);
  }

  public static List<EntityGroupRank> getEntityRankStatistically(int operatorId, Date date) throws PersistenceException {
    return getRanked(computeWeightForEntities(getRecentHitsOfOperator(operatorId,date)), 20);
  }

  public static List<EntityGroupRank> getEntityRankByFreshness(int operatorId, Date date) throws PersistenceException {
    List<EntityGroupRank> grr = new LinkedList<EntityGroupRank>();
    List<Hit> hits=getRecentHitsOfOperator(operatorId,date);
    Set<String> visited = new HashSet();
    int i=0;
    for (Hit hit:hits){
      String classPlusId = hit.getEntityClass() + "^" + hit.getEntityId();
      if (!visited.contains(classPlusId)){
        visited.add(classPlusId);
        EntityGroupRank egr = new EntityGroupRank();
        //egr.classAndId = classPlusId;
        egr.className=hit.getEntityClass();
        egr.id= hit.getEntityId();
        egr.weight = CompanyCalendar.MILLIS_IN_DAY/(System.currentTimeMillis()-hit.getWhen());
        Identifiable entity = egr.getEntity();
        if (entity != null) {
          grr.add(egr);
          i++;
        }
      }
      if (i >= 20)
        break;

    }
    return grr;
  }

  public static Map<String, List<EntityGroupRank>> splitByClassName(List<EntityGroupRank> entityGroupRanks) {
    Map<String, List<EntityGroupRank>> grouped = new HashTable<String, List<EntityGroupRank>>();

    for (EntityGroupRank egr : entityGroupRanks) {
      List<EntityGroupRank> leg;

      String className;
      if (ReflectionUtilities.instanceOf(egr.getEntity(), Resource.class))
        className = Resource.class.getName();
      else
        className = ReflectionUtilities.deProxy(egr.getEntity().getClass().getName());

      if (grouped.containsKey(className)) {
        leg = grouped.get(className);
      } else {
        leg = new ArrayList();
        grouped.put(className, leg);

      }
      leg.add(egr);
    }
    return grouped;
  }
}
