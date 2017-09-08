package org.jblooming.ontology;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.html.layout.HtmlColors;

import java.io.Serializable;
import java.util.*;


public abstract class PerformantNodeSupport extends PerformantNodeBean implements PerformantNode {

  protected PerformantNodeSupport parent;
  private Set<PerformantNodeSupport> children = new HashSet<PerformantNodeSupport>();

  public PerformantNodeSupport() {
    super();
  }

  //PersistenceContext
  public void setParentAndStore(PerformantNodeSupport n) {

    //I must not be in n ancestors
    if (n != null && n.getAncestorIds() != null && n.getAncestorIds().trim().length() > 0 &&
      ((n.getAncestorIds().indexOf(SEPARATOR + this.getId() + SEPARATOR) != -1) || (n.getAncestorIds().startsWith(this.getId() + SEPARATOR)))
      )
      throw new PlatformRuntimeException(
        "The parent node cannot be in this node's descendant:" +
          " parent (id " + n.getId() + ") ancestor ids = '" + n.getAncestorIds() + "' include this (id " + this.getId() + ")");

    //remove from old parent children
    if(getParentNode() != null && !getParentNode().equals(n)){
      getParentNode().getChildrenNode().remove(this);
    }

    setParentNode(n);
    if (n != null)
      n.addChild(this);

    if (n != null)
      setAncestorIds((n.getAncestorIds() != null ? n.getAncestorIds() : "") + n.getId() + SEPARATOR);
    else
      setAncestorIds(null);
    
    if (JSP.ex(getChildrenNode() != null))
      for (Object o : getChildrenNode()) {
        PerformantNodeSupport child = (PerformantNodeSupport) o;
        child.setParentAndStore(this);
      }
  }

  public Iterator<PerformantNodeSupport> getChildrenIterator() {
    Iterator i;
    if (children != null)
      i = children.iterator();
    else
      i = Collections.EMPTY_SET.iterator();
    return i;
  }


  public Iterator<PerformantNodeSupport> getChildrenIterator(Comparator comp) {

    Iterator i;
    if (children != null) {
      List sort = new ArrayList(children);
      Collections.sort(sort, comp);
      i = sort.iterator();
    } else
      i = Collections.EMPTY_SET.iterator();
    return i;
  }


  public Iterator<PerformantNodeSupport> getChildrenIteratorByName() {

    return getChildrenIterator(new PlatformComparators.NameComparator());
  }


  public int[] getAncestorIdsList() {
    int[] intIds = null;
    if (ancestorIds != null) {
      List stringIds = StringUtilities.splitToList(ancestorIds, SEPARATOR);
      if (stringIds != null && stringIds.size() > 0) {
        intIds = new int[stringIds.size() - 1];
        for (int i = 0; i < stringIds.size() - 1; i++) {
          intIds[i] = Integer.parseInt((String) stringIds.get(i));
        }
      }
    }
    return intIds;
  }


  public List<String> getAncestorIdsAsList() {
    if (ancestorIds != null) {
      List<String> strings = StringUtilities.splitToList(ancestorIds, SEPARATOR);
      if ("".equals(strings.get(strings.size()-1).trim()))
        strings.remove(strings.size()-1);
      return strings;
    } else
      return new ArrayList();
  }

  /**
   * list does not include this node id
   * @param subClazz
   */
  public List<? extends PerformantNodeSupport> getDescendants(Class subClazz) throws FindException {
    String hql = "from " + subClazz.getName() + " as pns where pns.ancestorIds like :myids_stemmed";
    return getPerformantly(hql);
  }

  private List getPerformantly(String hql) throws FindException {
    OqlQuery oql = new OqlQuery(hql);
    String param = (ancestorIds != null ? ancestorIds : "") + id + SEPARATOR + "%";
    oql.getQuery().setString("myids_stemmed", param);
    List list = oql.list();
    return list;
  }

  /**
   * list does not include this node id
   * @param subClazz
   */
  public List<Serializable> getDescendantIds(Class subClazz) throws FindException {
    String hql = "select pns.id from " + subClazz.getName() + " as pns where pns.ancestorIds like :myids_stemmed";
    return getPerformantly(hql);
  }

  public int getDescendantsSize(Class subClazz) throws FindException {
    String hql = "select count(pns.id) from " + subClazz.getName() + " as pns where pns.ancestorIds like :myids_stemmed";
    return ((Long) getPerformantly(hql).iterator().next()).intValue();
  }


  public int getMaxDepthOfDescendants(Class subClazz) throws FindException {
    int result = 0;
    for (Serializable id : getDescendantIds(subClazz)) {
      result = Math.max(result, StringUtilities.occurrences(id.toString(), SEPARATOR));
    }
    return result;
  }

  /**
   *
   * @return a list of encestors including this
   */
  public List getAncestors() {

    if (this.getParentNode() == null) {
      List recursor = new ArrayList();
      recursor.add(this);
      return recursor;
    } else {
      List recursor = ((PerformantNodeSupport) this.getParentNode()).getAncestors();
      recursor.add(this);
      return recursor;
    }
  }

  public static void recalculatePerformantNodesAndStore(PerformantNodeSupport rootNode) throws StoreException {
    if (rootNode != null) {
      List<Node> ancs = rootNode.getAncestors();
      for (int i = 0; i < ancs.size(); i++) {
        PerformantNodeSupport node = (PerformantNodeSupport) ancs.get(i);
        if (node.getParentNode() != null)
          node.setParentAndStore((PerformantNodeSupport) node.getParentNode());
        node.store();
      }
    }
  }

  public static void recalculatePerformantNodesAndStore(Set<? extends PerformantNodeSupport> nodes) throws StoreException {

    for (PerformantNodeSupport performantNode : nodes) {
      List<Node> ancs = performantNode.getAncestors();
      for (int i = 0; i < ancs.size(); i++) {
        PerformantNodeSupport node = (PerformantNodeSupport) ancs.get(i);
        if (node.getParentNode() != null)
          node.setParentAndStore((PerformantNodeSupport) node.getParentNode());
        node.store();
      }
    }
  }

  public PerformantNodeSupport getAncestor() {

    if (this.getParentNode() == null) {
      return this;
    } else {
      return ((SecuredNodeSupport) this.getParentNode()).getAncestor();
    }
  }


  public int getDepth() {
    int[] ancestorIdsList = getAncestorIdsList();
    if (ancestorIdsList != null)
      return ancestorIdsList.length;
    else
      return 0;
  }


  public Set<PerformantNodeSupport> getChildren() {
    return children;
  }

  private void setChildren(Set<PerformantNodeSupport> children) {
    this.children = children;
  }

  public Collection getChildrenNode() {
    return getChildren();
  }

  public void addChild(PerformantNodeSupport pns) {
    getChildren().add(pns);
  }

  public int getChildrenSize() {
    return getChildren() != null ? getChildren().size() : 0;
  }

  public boolean childrenContains(Node child) {
    return getChildren().contains(child);
  }

  public int childrenSize() {
    return getChildren().size();
  }


  public abstract Node getParentNode();

  public abstract void setParentNode(Node node);



  public PerformantNodeSupport getPreviousBrother() {
    PerformantNodeSupport brother = null;
    PerformantNodeSupport parent = (PerformantNodeSupport) getParentNode();
    if (parent != null && parent.getChildrenSize() > 1) {
      Iterator i = parent.getChildrenIteratorByName();
      while (i.hasNext()) {
        PerformantNodeSupport node = (PerformantNodeSupport) i.next();
        if (node.equals(this))
          break;
        brother = node;
      }

    }
    return brother;
  }

  public PerformantNodeSupport getNextBrother() {
    PerformantNodeSupport brother = null;
    PerformantNodeSupport parent = (PerformantNodeSupport) getParentNode();
    boolean foundMyself = false;
    if (parent != null && parent.getChildrenSize() > 1) {
      Iterator i = parent.getChildrenIteratorByName();
      while (i.hasNext()) {
        PerformantNodeSupport task = (PerformantNodeSupport) i.next();
        if (foundMyself) {
          brother = task;
          break;
        }
        if (task.equals(this))
          foundMyself = true;
      }
    }
    return brother;
  }

  public List<PerformantNodeSupport> getBrothers() {
    List<PerformantNodeSupport> brothers = new ArrayList<PerformantNodeSupport>();
    PerformantNodeSupport parent = (PerformantNodeSupport) getParentNode();
    if (parent != null && parent.getChildrenSize() > 1) {
      Iterator i = parent.getChildrenIteratorByName();
      while (i.hasNext()) {
        PerformantNodeSupport child = (PerformantNodeSupport) i.next();
        if (!child.equals(this)) {
          brothers.add(child);
        }
      }
    }
    return brothers;
  }

  public String getPath(String separator, boolean showSelf) {
    List<PerformantNodeSupport> ancs = getAncestors();

    String path = "";
    boolean first = true;
    for (PerformantNodeSupport anc : ancs) {
      if (!showSelf && anc==this)
        break;
      path = path + (first ? "" : separator) + anc.getName();
      first = false;

    }
    return path;
  }

  public String getDepthColor() {
    return HtmlColors.getDepthColor(this.getDepth());
  }

  public String getChildAncentorIds(){
     return this.getAncestorIds() == null ? getId() + PerformantNode.SEPARATOR : this.getAncestorIds() + getId() + PerformantNode.SEPARATOR;
  }

  public String getAncestorsPath(String separator){
    String p=this.getName();
    Node n= this.getParentNode();
    while (n.getParentNode()!=null){
      n=this.getParentNode();
      p=n.getName()+separator+p;
    }
    return p;
  }

}
