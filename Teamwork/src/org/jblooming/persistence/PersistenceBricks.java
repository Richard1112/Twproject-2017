package org.jblooming.persistence;

import org.hibernate.SessionFactory;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.utilities.CodeValueList;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.Bricks;
import org.jblooming.waf.settings.PersistenceConfiguration;
import org.jblooming.ontology.IdentifiableSupport;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Dec 18, 2007
 * Time: 3:40:41 PM
 */
public class PersistenceBricks extends Bricks {

  public static CodeValueList getPersistentEntities(Class clazzToBeExtended) {

    PersistenceConfiguration persistenceConf = PersistenceConfiguration.getInstance(clazzToBeExtended);
    if (persistenceConf==null)
      persistenceConf=PersistenceConfiguration.getDefaultPersistenceConfiguration();
    SessionFactory sf = persistenceConf.getSessionFactory();


    Map acm = sf.getAllClassMetadata();
    Set keysAcm = acm.keySet();

    CodeValueList cvl = new CodeValueList();

    for (Iterator iterator = keysAcm.iterator(); iterator.hasNext();) {
      String className = (String) iterator.next();
      //it may be an entity name, and not a class
      Class persClass = null;
      try {
        persClass = Class.forName(className);
        if (clazzToBeExtended == null || ReflectionUtilities.extendsOrImplements(persClass, clazzToBeExtended))
          cvl.add(className, persClass.getSimpleName());
      } catch (ClassNotFoundException e) {
      }
    }
    return cvl;
  }
}
