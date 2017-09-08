<%@ page import="org.hibernate.SessionFactory, org.hibernate.engine.CascadesProxy, org.hibernate.persister.collection.AbstractCollectionPersister, org.hibernate.type.Type,
org.jblooming.ontology.Identifiable, org.jblooming.ontology.Node, org.jblooming.oql.OqlQuery, org.jblooming.utilities.JSP, org.jblooming.utilities.ReflectionUtilities,org.jblooming.utilities.StringUtilities,
 org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.ObjectEditorConstants, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.container.ButtonBar,
 org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.input.RadioButton, org.jblooming.waf.settings.I18n,
  org.jblooming.waf.settings.PersistenceConfiguration, org.jblooming.waf.view.PageState,java.util.*" %>
<%

  DeletePreviewer deletePreviewer = (DeletePreviewer) JspIncluderSupport.getCurrentInstance(request);

  PageState pageState = PageState.getCurrentPageState(request);


  Identifiable delendo = deletePreviewer.delendo;
  //necessary to initialize and hence inspect all data
  delendo = ReflectionUtilities.getUnderlyingObject(delendo);

  boolean foundSomeReference = false;

  boolean delendaIsNode = delendo instanceof Node;
  boolean delendaIsRoot = !delendaIsNode || ((Node) delendo).getParentNode() == null;
  boolean delendaHasChildren = delendaIsNode && ((Node) delendo).getChildrenNode().iterator().hasNext();
  Class objClass = delendo.getClass();
  String clazzName = ReflectionUtilities.deProxy(objClass.getName());
  objClass = Class.forName(clazzName);
  PersistenceConfiguration persistenceConf = PersistenceConfiguration.getInstance(objClass);
  SessionFactory sf = persistenceConf.getSessionFactory();

  String[] names = ReflectionUtilities.getPropertyNames(delendo);

  String humanName = "<small>(id:"+ delendo.getId() +")</small> " + JSP.limWr(JSP.encode(delendo.getName()), 150);

%>

<%=I18n.get("DELETE_PREVIEW", humanName)%>
<table class="table" id="collections"><%

  // -----------------------------------------------------------------------  COLLECTIONS   ------------------------------------------------------------------------------------

  CascadesProxy csp = new CascadesProxy();
  for (int i = 0; i < names.length; i++) {
    String name = names[i];
    Object propertyValue = ReflectionUtilities.getFieldValue(name, delendo); //entityPersister.getPropertyValue(delendo, name, HibernateFactory.getSession().getEntityMode());

    if (propertyValue != null) {
      if (propertyValue instanceof Collection && ((Collection) propertyValue).size() > 0 && !csp.doesCascadeOnDelete(i, delendo)) {
        Collection coll = (Collection) propertyValue;
        Object sample = coll.iterator().next();

        if (sample instanceof Identifiable) {
          foundSomeReference = true;
          boolean membersAreNodesOfTheSameFamily = sample instanceof Node && ReflectionUtilities.instanceOf(sample, (deletePreviewer.normalizeInstanceToSuperclass != null ? deletePreviewer.normalizeInstanceToSuperclass : delendo.getClass()));
          if (membersAreNodesOfTheSameFamily) {
            %>
            <tr height="40">
              <td><%=I18n.get("COLLECTION_REFERENCE")%>:<b>"<%=name%>"</b>&nbsp;<%=I18n.get("MOVE_TO_ROOT_OR_DELETE")%></td>
            </tr>
            <%

              RadioButton rbGlobalUnlink = new RadioButton(I18n.get("UNLINK"), ObjectEditorConstants.FLD_DELETE_STYLE + "__" + name, Commands.UNLINK, "", "", false, "");
              RadioButton rbGlobalMove = new RadioButton(I18n.get("MOVE_TO_PARENT"), ObjectEditorConstants.FLD_DELETE_STYLE + "__" + name, Commands.UP, "", "", false, "");
              RadioButton rbGlobalDelRec = new RadioButton(I18n.get("DELETE_DESCENDANTS"), ObjectEditorConstants.FLD_DELETE_STYLE + "__" + name, Commands.DELETE_DESCENDANTS, "", "", false, "");

              if (coll.size() > 0) {
                %>
                <tr>
                  <td>
                    <table class="table">
                      <tr>
                        <td width="40"</td>
                        <td width="150" height="25" valign="middle"><b><%=coll.size()%> "<%=name%>"</b></td>
                        <td><b><%rbGlobalUnlink.toHtml(pageContext);%></b> <br>
                        <%
                          if (!delendaIsRoot) {
                            //sposta
                            %><b><%rbGlobalMove.toHtml(pageContext);%></b><br><%
                          }

                          if (membersAreNodesOfTheSameFamily) {
                            //elimina figli
                            %><b><%rbGlobalDelRec.toHtml(pageContext);%></b><%
                          }
                        %></td>
                      </tr>
                    </table>
                  </td>
                </tr>
                <%
             }
          }
        }
      }
    }
  }



// -----------------------------------------------------------------------  REFERRERS   ------------------------------------------------------------------------------------
%>
</table>
<br>
<table  class="table" id="referrers">
  <tbody  style="color: #444">
<%

  // case when referrer has delendo as property
  Map acm = sf.getAllClassMetadata();

  if (acm!= null && acm.size() > 0) {
    Set<String> keysAcm = new TreeSet<String>(new Comparator<String>(){
      public int compare(String o1, String o2) {
        if (o1.equals(o2))
          return 0;
        return (o1.substring(o1.lastIndexOf(".")+1)).compareTo(o2.substring(o2.lastIndexOf(".")+1));
        //return o1.compareTo(o2);
      }
    });
    keysAcm.addAll(acm.keySet());

    for (String className:keysAcm ) {
      //it may be an entity name, and not a class
      Class persClass = null;
      try {
        persClass = Class.forName(className);
      } catch (ClassNotFoundException e) {
      }

      if (persClass != null && !persClass.equals(delendo.getClass())) {


        Type[] types = ReflectionUtilities.getPropertyTypes(className);
        List camps = new ArrayList();
        for (int i = 0; i < types.length; i++) {
          Type type = types[i];
          //verify that it works on hierarchies
          if (delendo.getClass().equals(type.getReturnedClass())) {
            //potentially refers delendo
            camps.add(ReflectionUtilities.getPropertyNames(className)[i]);
          }
        }
        if (camps.size() > 0) {
          String hql = "select count(o.id) from " + persClass.getName() + " as o where ";
          for (Iterator iterator1 = camps.iterator(); iterator1.hasNext(); ) {
            String s = (String) iterator1.next();
            hql = hql + "o." + s + " = :delendo or ";
          }
          hql = hql.substring(0, hql.length() - 4);
          OqlQuery oql = new OqlQuery(hql);
          oql.getQuery().setEntity("delendo", delendo);
          long size = ((Long) oql.uniqueResult()).intValue();
          if (size > 0) {
            foundSomeReference = true;
            final String pcm = persClass.getName();
            %>
            <tr>
              <td><b><%=StringUtilities.deCamel(pcm.substring(pcm.lastIndexOf(".") + 1))%></b> <%=I18n.get("DELETE_REFERENCES_IN_FIELD", size + "", camps.get(0).toString())%>
              </td>
            </tr>
            <%
          }
        }
      }
    }
  }

  // case when referrer has delendo as member of collection
  Map collMeta = sf.getAllCollectionMetadata();
  Set keysCollMeta = collMeta.keySet();

  if (keysCollMeta != null && keysCollMeta.size() > 0) {
    for (java.util.Iterator iterator = keysCollMeta.iterator(); iterator.hasNext(); ) {
      String persCollFullName = (String) iterator.next();
      AbstractCollectionPersister cm = (AbstractCollectionPersister) collMeta.get(persCollFullName);

      final Class returnedClass = cm.getElementType().getReturnedClass();
      // I want to find delendo in collections not in delendo
      Class ownerClass = cm.getElementClass();//getOwnerClass();
      if (ownerClass != null && !ownerClass.equals(delendo.getClass()) && returnedClass.equals(delendo.getClass())) {
        // candidate found
        String hql = "select count(o.id) from " + ownerClass.getName() + " as o left join o." +
          cm.getName().substring(cm.getName().lastIndexOf(".") + 1) + " as prop where prop = :delendo ";

        //select user
        //from User as user
        //left join user.messages as msg

        OqlQuery oql = new OqlQuery(hql);
        oql.getQuery().setEntity("delendo", delendo);
        int size = ((Integer) oql.uniqueResult()).intValue();
        if (size > 0) {
          foundSomeReference = true;
          %>
          <tr>
            <td>
              <b><%=size%></b> references found in collection
              "<%=cm.getName().substring(cm.getName().lastIndexOf(".") + 1).toLowerCase()%>" of
              "<%=ownerClass.getName().substring(ownerClass.getName().lastIndexOf(".") + 1).toLowerCase() %>".
            </td>
          </tr>
          <%
        }
      }
    }
  }

%>
<tr align="center">
  <td valign="middle"><%--

    if (!foundSomeReference) {
      %><%=I18n.get("NO_DIRECT_REFERENCES_FOUND")%><%
    }
--%>
<%

// -----------------------------------------------------------------------  BUTTONS   ------------------------------------------------------------------------------------

  %></td>
</tr>
<tr align="right">
  <td valign="middle"><%

    ButtonBar butBar = new ButtonBar();

    ButtonJS bs = new ButtonJS("performDelete('"+deletePreviewer.id+"',"+ delendo.getId()+","+(pageState.getEntry("ISCALLBACK").checkFieldValue())+");");
    bs.additionalCssClass = "delete";
    bs.label = I18n.get("DELETE_AND_APPLY");
    butBar.addButton(bs);


  %></td>
</tr>
  </tbody>
</table>
<%butBar.toHtml(pageContext);%>
<div id="deleteErrorFeedback" style="display: none;"><b style="color:#ff0000;"><%=I18n.get("DPRBOBJ","")%></b><br><code style="font-size: 70%;color: #ccc"></code></div>
