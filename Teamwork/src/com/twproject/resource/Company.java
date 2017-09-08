package com.twproject.resource;

import com.opnlb.fulltext.Indexable;
import com.twproject.operator.TeamworkOperator;
import org.apache.lucene.analysis.core.StopAnalyzer;
import org.hibernate.search.annotations.*;
import org.hibernate.search.bridge.builtin.StringBridge;
import org.jblooming.anagraphicalData.AnagraphicalData;
import org.jblooming.company.DepartmentType;
import org.jblooming.designer.DesignerData;
import org.jblooming.utilities.JSP;

import java.io.Serializable;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
@Indexed(index = "fulltext")
@Boost(1.5f)
public class Company extends Resource implements Indexable {

  /**
   * determines the department type, e.g. Company, Department, Office
   */
  private DepartmentType type;


  public DepartmentType getType() {                    
    return type;
  }


  @DocumentId
  @FieldBridge(impl= StringBridge.class)
  public Serializable getId() {
      return super.getId();
  }

  public void setType(DepartmentType type) {
    this.type = type;
  }

  public Set<Person> getPersons(Set<Resource> visitedNodes, Set<Person> workers) {

    if (!visitedNodes.contains(this)) {

      visitedNodes.add(this);

      // visiting parent node

      if (isInherit() && getParent() != null) {
        workers.addAll((getParent()).getPersons(visitedNodes, workers));
      }

      // visiting children

      if (isPropagate()) {
        Iterator iter = getChildrenIterator();
        Resource t;
        while (iter.hasNext()) {
          t = (Resource) iter.next();
          workers.addAll( t.getPersons(visitedNodes, workers));
        }
      }
    }
    return workers;
  }



  public boolean isPersonIn(Person o) {
    return getPersons().contains(o);
  }


  public String getDisplayName() {
    return getName();
  }

  public TeamworkOperator getMyself() {
    return null;
  }

  public String getAbstractForIndexing() {

    String resourceAbstract=super.getAbstractForIndexing();
    return resourceAbstract;
  }

  @Fields({
          @Field(name = "fullcontent", analyzer = @Analyzer(impl = StopAnalyzer.class)),
          @Field(name = "content")
  })
  private String getContentForIndexing() {
    return getAbstractForIndexing();
  }


}
