package org.jblooming.flowork;

import org.jbpm.persistence.db.DbPersistenceServiceFactory;
import org.hibernate.cfg.Configuration;
import org.jblooming.persistence.hibernate.HibernateFactory;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Aug 20, 2007
 * Time: 3:43:07 PM
 */
public class FloworkPersistenceConfigurationFucktory extends DbPersistenceServiceFactory {

   public synchronized Configuration getConfiguration() {
    return HibernateFactory.getConfig();
  }

}
