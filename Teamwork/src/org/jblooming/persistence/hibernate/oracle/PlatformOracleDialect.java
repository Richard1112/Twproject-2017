package org.jblooming.persistence.hibernate.oracle;

import org.hibernate.dialect.Oracle10gDialect;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class PlatformOracleDialect extends Oracle10gDialect {

  public Class getNativeIdentifierGeneratorClass() {
    return PlatformSequenceGenerator.class;
  }

}
