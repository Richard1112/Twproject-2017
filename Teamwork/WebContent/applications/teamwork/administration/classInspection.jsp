<%@ page import="com.twproject.agenda.Event,
                 com.twproject.operator.TeamworkOperator,
                 com.twproject.resource.Resource,
                 com.twproject.task.Assignment,
                 com.twproject.task.Issue,
                 com.twproject.task.Task,
                 com.twproject.task.TaskPersistent,
                 com.twproject.task.financial.Cost,
                 com.twproject.waf.TeamworkPopUpScreen,
                 com.twproject.worklog.Worklog,
                 org.jblooming.agenda.Period,
                 org.jblooming.ontology.Identifiable,
                 org.jblooming.utilities.CollectionUtilities,
                 org.jblooming.utilities.DateUtilities,
                 org.jblooming.utilities.ReflectionUtilities,
                 org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 sun.reflect.generics.reflectiveObjects.WildcardTypeImpl,
                 java.lang.reflect.Field,
                 java.lang.reflect.Method, java.lang.reflect.ParameterizedType, java.lang.reflect.Type, java.util.Comparator, java.util.List, java.util.Set, java.util.TreeSet, com.twproject.resource.Person, com.twproject.resource.Company" %><%


PageState pageState = PageState.getCurrentPageState(request);
if (!pageState.screenRunning) {
  pageState.screenRunning = true;
  final ScreenArea body = new ScreenArea(request);
  TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
  lw.register(pageState);
  pageState.perform(request, response).toHtml(pageContext);
} else {
  TeamworkOperator loggedOperator = (TeamworkOperator) pageState.getLoggedOperator();
  if (!loggedOperator.hasPermissionAsAdmin())
    return;

  //header
  List<Class<?>> objects = CollectionUtilities.toList(Task.class, Person.class, Company.class, Assignment.class, Worklog.class, Issue.class, Cost.class, Period.class, Event.class, DateUtilities.class, StringUtilities.class);
  for (Class c : objects) {
    getClassButton(c, pageState, request).toHtmlInTextOnlyModality(pageContext);
    %>&nbsp;&nbsp;<%
  }


  String className = pageState.getEntry("FOCUSED_CLASS").stringValueNullIfEmpty();
  Class clazz;
  try {
    clazz = Class.forName(className);
  } catch (Throwable t) {
    clazz = Task.class;
  }
  Class subclass = clazz;
  Class superclass = subclass.getSuperclass();



  boolean isPersisted = ReflectionUtilities.extendsOrImplements(clazz, Identifiable.class);


%>
<hr>
<table class="table">
  <tr>
    <td colspan="2" style="padding-bottom: 25px;"><h1 style="margin:0;"><%=clazz.getSimpleName()%></h1>(<%=clazz.getCanonicalName()%>)<br></td>
  </tr>
  <tr class="tableHead">
    <th><h2>Fields</h2></th>
    <th><h2>Getters</h2></th>
  </tr>
  <tr>
    <td valign="top"><%

      //if (isPersisted) {
        Set<Field> publicFields = new TreeSet<Field>(new Comparator<Field>() {
          public int compare(Field o1, Field o2) {
            return o1.getName().compareTo(o2.getName());
          }
        });

        publicFields.addAll(CollectionUtilities.toList(clazz.getDeclaredFields()));

        if (superclass != null && isOneOfOurs(superclass)) {
          publicFields.addAll(CollectionUtilities.toList(superclass.getDeclaredFields()));
        }

        for (Field publicField : publicFields) {
          String fieldName = publicField.getName();
          Class typeClass = publicField.getType();
          String fieldType = typeClass.getSimpleName();

          %><b><%=fieldName%></b>: &nbsp;<%

    if (isOneOfOurs(typeClass)) {
            getClassButton(typeClass, pageState, request).toHtmlInTextOnlyModality(pageContext);
          } else {
    %><%=fieldType%><%

      //vediamo se è un generic tipo set<Assignment>
      if (publicField.getGenericType() instanceof ParameterizedType) {
    %>&nbsp;&lt;<%
      getClassButton((Class) ((ParameterizedType) publicField.getGenericType()).getActualTypeArguments()[0], pageState, request).toHtmlInTextOnlyModality(pageContext);
    %>&gt;<%

        }
      }
    %><br><%
        }
     // }
    %></td>
    <td valign="top"><%

      Set<Method> theMethods = new TreeSet<Method>(new Comparator<Method>() {
        public int compare(Method o1, Method o2) {
          return o1.getName().compareTo(o2.getName());
        }
      });
      theMethods.addAll(CollectionUtilities.toList(clazz.getMethods()));

      if (superclass != null && isOneOfOurs(superclass)) {
        theMethods.addAll(CollectionUtilities.toList(superclass.getMethods()));
      }

      for (Method theMethod : theMethods) {
        Class<?> returnType = theMethod.getReturnType();
        String returnString = returnType.getSimpleName();
        String methodString = theMethod.getName();
        if ((!methodString.startsWith("get") && isPersisted) || methodString.startsWith("set") || "void".equals(returnString))
          continue;

        %><b><%=methodString%> </b>( <%
        Class[] parameterTypes = theMethod.getParameterTypes();
        for (Class parameterType : parameterTypes) {
          %><%=parameterType.getSimpleName()%>&nbsp;<%
        }
        %>)&nbsp;->&nbsp; <%

        if (isOneOfOurs(returnType)) {
          getClassButton(returnType, pageState, request).toHtmlInTextOnlyModality(pageContext);
        } else {
          %><%=returnString%><%
          //vediamo se è un generic tipo set<Assignment>
          if (theMethod.getGenericReturnType() instanceof ParameterizedType) {
            Type type = ((ParameterizedType) theMethod.getGenericReturnType()).getActualTypeArguments()[0];
            if (!(type instanceof WildcardTypeImpl)) {
              %>&nbsp;&lt;<%getClassButton((Class) type, pageState, request).toHtmlInTextOnlyModality(pageContext);%>&gt;<%
            }
          }
        }
        %><br><%
      }
    %></td>
  </tr>
</table>
<%

  }
  // }
%><%!

  private boolean isOneOfOurs(Class clazz) {
    return clazz != null && clazz.getCanonicalName().contains(".twproject") || clazz.getCanonicalName().contains(".opnlb") || clazz.getCanonicalName().contains(".jblooming");
  }

  private ButtonLink getClassButton(Class clazz, PageState pageState, HttpServletRequest request) {
    PageSeed ps = pageState.thisPage(request);
    ps.addClientEntry("FOCUSED_CLASS", clazz.getName());
    return new ButtonLink(clazz.getSimpleName(), ps);
  }
%>