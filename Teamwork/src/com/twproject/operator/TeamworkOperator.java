package com.twproject.operator;

import com.twproject.rank.Hit;
import com.twproject.resource.Person;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.TeamworkArea;
import net.sf.json.JSONObject;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.PlatformComparators;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.Area;
import org.jblooming.security.Permission;
import org.jblooming.security.PlatformPermissions;
import org.jblooming.security.Role;
import org.jblooming.utilities.CollectionUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.view.PageState;

import javax.persistence.Transient;
import java.io.Serializable;
import java.util.*;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class TeamworkOperator extends Operator {

  private Person myPerson; // inverse

  public TeamworkOperator(){
    super();
  }


  public Person getPerson() {
    return myPerson;
  }

  private void setPerson(Person myPerson) {
    this.myPerson = myPerson;
  }


  public static TeamworkOperator load(Serializable id) throws FindByPrimaryKeyException {
    return (TeamworkOperator) PersistenceHome.findByPrimaryKey(TeamworkOperator.class, id);
  }


  public Set<Area> getAreasForPermission(Permission aPermission) throws PersistenceException {

    Set<Area> areas = new TreeSet<Area>(new PlatformComparators.NameComparator());

    areas.addAll(getAreasOwned());
    for (Role r : getInheritedRoles()) {
      if (aPermission == null || r.getPermissions().contains(aPermission)) {
        RoleTeamwork tr = (RoleTeamwork) PersistenceHome.findByPrimaryKey(RoleTeamwork.class, r.getId()); //todo ma serve il find again?
        if (tr != null && tr.getArea() != null && !areas.contains(tr.getArea()))
          areas.add(tr.getArea());
      }
    }
    return areas;
  }

  public Set<Area> getAreasForPermissionPlusMine(Permission aPermission) throws PersistenceException {
    Set<Area> a = getAreasForPermission(aPermission);
    Area myArea = getPerson().getArea();
    if (myArea != null)
      a.add(myArea);
    return a;
  }


  public boolean isAreaManager() {
    try {
      return getAreasOwned().size() > 0 || getAreasForPermission(PlatformPermissions.area_canManage).size() > 0;
    } catch (PersistenceException e) {
      throw new PlatformRuntimeException(e);
    }
  }

  public Set<Area> getAreasOwned() throws FindException {
    //add all those which you own
    String hql = "from " + TeamworkArea.class.getName() + " as area ";

    if (!hasPermissionAsAdmin())
      hql = hql + "where area.owner = :myself";

    OqlQuery oql = new OqlQuery(hql);
    if (!hasPermissionAsAdmin())
      oql.getQuery().setEntity("myself", this);
    return new HashSet<Area>(oql.list());
  }

  public Area getDefaultAreaForPermission(Permission aPermission) throws PersistenceException {
    Set<Area> areas = getAreasForPermission(aPermission);
    if (areas.size() == 0)
      return null;
    if (areas.size() == 1)
      return areas.iterator().next();
    else {
      Person myPersonFromPersistence = getPerson();
      if (myPersonFromPersistence != null && myPersonFromPersistence.getArea() != null && areas.contains(myPersonFromPersistence.getArea()))
        return myPersonFromPersistence.getArea();
      else {
        //find one which I own
        for (Area a : areas) {
          if (this.equals(a.getOwner()))
            return a;
        }
        // oth. return randomly
        return areas.iterator().next();
      }
    }
  }

  public String getDefaultEmail() {
    return getPerson().getDefaultEmail();
  }

  /**
   * @deprecated use hasPermissionAsAdmin
   */
  public boolean isAdministrator() {
    return super.isAdministrator();
  }


  public PersistentFile getImage() {
    return getPerson().getMyPhoto();
  }

  @Transient
  public int getScore() {
    String hql = "select sum(hit.weight) from " + Hit.class.getName() + " as hit where hit.when>:since and  hit.operatorId=:opId";
    OqlQuery oql = new OqlQuery(hql);
    CompanyCalendar cc = new CompanyCalendar();
    //should be same as recent hits of RankUtilities    
    cc.add(CompanyCalendar.MONTH, -1);
    cc.setAndGetTimeToDayStart();
    oql.getQuery().setLong("since", cc.getTime().getTime());
    oql.getQuery().setInteger("opId", getIntId());
    Object o = oql.uniqueResultNullIfEmpty();
    if (o != null)
      return ((Double) o).intValue();
    else
      return 0;
  }

  @Transient
  public List<Hit> getScoreDetail() throws FindException {
    String hql = "select hit from " + Hit.class.getName() + " as hit where hit.when>:since and  hit.operatorId=:opId order by hit.when desc";
    OqlQuery oql = new OqlQuery(hql);
    CompanyCalendar cc = new CompanyCalendar();
    //should be same as recent its of RankUtilities
    cc.add(CompanyCalendar.MONTH, -1);
    cc.setAndGetTimeToDayStart();
    oql.getQuery().setLong("since", cc.getTime().getTime());
    oql.getQuery().setSerializable("opId", getId());
    return oql.list();
  }


  public JSONObject jsonify() {
    JSONObject ret= super.jsonify();
    ret.element("person", getPerson().jsonify());
    return ret;
  }


  @Transient
  /**
   * if nothing found, uses default media, if not specified uses log
   */
  public Set<String> getPreferredMediaOrDefault(MessagingSystem.Media defaultMedia){
    Set<String> medias = new HashSet();

    String mss = getOption(OperatorConstants.MEDIA_PREFERRED_CHANNEL);
    if (JSP.ex(mss))
      medias.addAll(StringUtilities.splitToSet(mss, ","));

    if (!JSP.ex(medias)){
      // se ho passato un default lo uso, ma se è MAIL, guardo se l'utente ha una mail altrimenti lo converto in STICKY
      if (JSP.ex(defaultMedia)){
        if(MessagingSystem.Media.EMAIL.toString().equals(defaultMedia))
          medias.add(JSP.ex(getPerson().getDefaultEmail()) ?MessagingSystem.Media.EMAIL.toString() : MessagingSystem.Media.STICKY.toString());
        else
          medias.add(defaultMedia.toString());

      //se non ho passato un default use LOG
      } else {
        medias.add(MessagingSystem.Media.LOG.toString());
      }

    }

    return medias;
  }





  // usato solo per costruire un fake operator saltando l'inversa
  public TeamworkOperator(Person myPerson){
    super();
    this.setPerson(myPerson);
  }

}
