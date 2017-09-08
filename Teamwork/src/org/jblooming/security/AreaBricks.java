package org.jblooming.security;

import org.jblooming.waf.Bricks;
import org.jblooming.waf.html.input.SmartCombo;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class AreaBricks extends Bricks {

  public Area mainObject;

  public AreaBricks(Area r) {
    this.mainObject = r;
  }


  public static SmartCombo getAllAreas(String fieldName, String additionalHql) {

    String hql = "select area.id, area.name from " + Area.class.getName() + " as area ";

    String whereForFiltering =
      "where area.name like :" + SmartCombo.FILTER_PARAM_NAME;

    String whereForId = "where area.id = :" + SmartCombo.FILTER_PARAM_NAME;

    if (additionalHql!=null)
      whereForFiltering = whereForFiltering + additionalHql;

    whereForFiltering = whereForFiltering + " order by area.name";

    SmartCombo areas = new SmartCombo(fieldName, hql, whereForFiltering, whereForId);

    areas.separator = "</td><td>";
    areas.fieldSize = 40;

    return areas;
  }



}
