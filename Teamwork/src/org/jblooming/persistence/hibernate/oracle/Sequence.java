package org.jblooming.persistence.hibernate.oracle;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class Sequence {

      public Class superClass;
      public String name;
      public String sql;
      public String parameters;

  public Sequence(Class superClass,String name) {
    this.superClass = superClass;
    this.name = name;
  }


}


