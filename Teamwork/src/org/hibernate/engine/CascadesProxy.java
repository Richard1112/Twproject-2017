package org.hibernate.engine;

import org.hibernate.Hibernate;
import org.hibernate.SessionFactory;
import org.hibernate.engine.spi.CascadeStyle;
import org.hibernate.engine.spi.CascadingAction;
import org.hibernate.engine.spi.CascadingActions;
import org.hibernate.persister.entity.SingleTableEntityPersister;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.persistence.hibernate.PersistenceContext;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class CascadesProxy implements CascadeStyle {

  public boolean doCascade(CascadingAction cascadingAction) {
    return false;
  }

  @Override
  public boolean reallyDoCascade(CascadingAction cascadingAction) {
    return false;
  }

  @Override
  public boolean hasOrphanDelete() {
    return false;
  }

  public boolean doesCascadeOnDelete(int i, Object delendo) {

    PersistenceContext persistenceContext = PersistenceContext.get((IdentifiableSupport)delendo);
    SessionFactory sf = persistenceContext.persistenceConfiguration.getSessionFactory();

    SingleTableEntityPersister entityPersister = (SingleTableEntityPersister) sf.getClassMetadata(Hibernate.getClass(delendo));
    CascadeStyle[] cs = entityPersister.getPropertyCascadeStyles();
    CascadeStyle cascade = cs[i];

    return cascade.doCascade(CascadingActions.DELETE);
  }

  public boolean doesCascadeOnDelete(CascadeStyle cesso) {
    return cesso.doCascade(CascadingActions.DELETE);
  }


}
