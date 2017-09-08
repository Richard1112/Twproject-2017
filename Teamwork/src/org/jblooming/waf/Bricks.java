package org.jblooming.waf;

import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.ontology.Identifiable;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.html.button.ButtonSupport;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;

import java.io.Serializable;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public abstract class Bricks {


  public static final String REFERRAL_ID = "REFERRAL_ID";
  public static final String REFERRAL_TYPE = "REFERRAL_TYPE";
  public IdentifiableSupport mainObject;

  public static PageSeed addReferral(Serializable id, Class i, PageSeed ps) {
    return addReferral(id, i.getName(), ps);
  }

  public static PageSeed addReferral(Serializable id, String className, PageSeed ps) {
    ps.addClientEntry(REFERRAL_TYPE, className);
    ps.addClientEntry(REFERRAL_ID, id);
    return ps;
  }

  public static boolean isReferralOfType(Class i, PageSeed ps) {
    return i.getName().equals(ps.getEntry(REFERRAL_TYPE).stringValueNullIfEmpty());
  }

  public static PageSeed preserveReferral(PageSeed ps, PageState pageState) {

    String type = pageState.getEntry(REFERRAL_TYPE).stringValueNullIfEmpty();
    String id = pageState.getEntry(REFERRAL_ID).stringValueNullIfEmpty();
    //is there anything to preserve ?
    if (type != null && id != null) {
      addReferral(id, type, ps);
    }
    return ps;
  }

  public static IdentifiableSupport getReferral(PageSeed ps) {
    IdentifiableSupport is = null;
    String type = ps.getEntry(REFERRAL_TYPE).stringValueNullIfEmpty();
    String id = ps.getEntry(REFERRAL_ID).stringValueNullIfEmpty();
    //is there anything to preserve ?
    if (JSP.ex(type)  && JSP.ex(id)) {
      type = ReflectionUtilities.deProxy(type);
      try {
        is = (IdentifiableSupport) PersistenceHome.findByPrimaryKey((Class<? extends Identifiable>) Class.forName(type), id);
      } catch (FindByPrimaryKeyException e) {
        throw new PlatformRuntimeException(e);
      } catch (ClassNotFoundException e) {
        throw new PlatformRuntimeException(e);
      }
    }
    return is;
  }

  public static EntityViewerBricks.EntityLinkSupport getLinkSupportForEntity(Identifiable entity, PageState pageState) {
    EntityViewerBricks bricks = ApplicationState.entityViewers.get(ReflectionUtilities.deProxy(entity.getClass().getName()));
    if (bricks!=null)
      return bricks.getLinkSupportForEntity(entity,pageState);
    else
      return null;
    }

  public static ButtonSupport getButtonForEntity(Identifiable entity, PageState pageState) {
    EntityViewerBricks.EntityLinkSupport supportForEntity = getLinkSupportForEntity(entity, pageState);
    return supportForEntity!=null ? supportForEntity.bs : null;
  }

}
