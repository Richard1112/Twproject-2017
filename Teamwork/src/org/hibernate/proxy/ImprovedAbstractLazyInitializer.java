package org.hibernate.proxy;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class ImprovedAbstractLazyInitializer {

  public static Class getRealClass(AbstractLazyInitializer ali) {
    Class result = null;
    if (ali.getTarget()!=null)
      result = ali.getTarget().getClass();

    return result;

  }

}
