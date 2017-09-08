package org.jblooming.utilities;

import net.sf.json.JSONObject;
import org.hibernate.Hibernate;
import org.hibernate.HibernateException;
import org.hibernate.SessionFactory;
import org.hibernate.internal.util.ReflectHelper;
import org.hibernate.persister.entity.AbstractEntityPersister;
import org.hibernate.persister.entity.EntityPersister;
import org.hibernate.persister.entity.SingleTableEntityPersister;
import org.hibernate.proxy.AbstractLazyInitializer;
import org.hibernate.proxy.HibernateProxy;
import org.hibernate.type.Type;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.ontology.LookupSupport;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.view.ClientEntry;

import java.beans.Introspector;
import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.*;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class ReflectionUtilities {


  /**
   * Returns the hierarchy of extended classes and all their implemented interfaces
   *
   * @param claz
   */
  public static List<Class> getInheritedClasses(Class claz) {
    List<Class> set = new ArrayList<Class>();
    privateGetInheritedClasses(claz, set);
    return set;
  }

  public static boolean extendsOrImplements(Class main, Class interfaceOrHigherClass) {
    return getInheritedClasses(main).contains(interfaceOrHigherClass);
  }

  public static boolean directlyImplements(Class main, Class aninterface) {
    boolean result = false;
    Class[] interfaces = main.getInterfaces();
    for (Class anInterfaceInLoop : interfaces) {
      if (anInterfaceInLoop.equals(aninterface)) {
        result = true;
        break;
      }

    }
    return result;
  }

  private static void privateGetInheritedClasses(Class claz, List set) {

    set.add(claz);
    if (!claz.isPrimitive()) {
      // first the interfaces
      Class[] interfaces = claz.getInterfaces();
      for (Class anInterface : interfaces) {
        privateGetInheritedClasses(anInterface, set);
      }
      // then the parent
      Class parent = claz.getSuperclass();
      if (parent != null)
        privateGetInheritedClasses(parent, set);
    }
  }

  public static Map<String, Field> getDeclaredInheritedFields(Class claz) {
    return getDeclaredInheritedFields(claz, true);
  }

  public static Map<String, Field> getDeclaredInheritedFields(Class claz, boolean ignoreStatic) {

    //@useConcrete warning: this MUST be a set so that most contrete field types do not get overwritten by more abstract ones
    List<Class> set = new ArrayList<Class>();
    privateGetInheritedClasses(claz, set);

    Map<String, Field> flds = new HashMap<String, Field>();
    for (Class clazz : set) {
      for (int i = 0; i < clazz.getDeclaredFields().length; i++) {
        Field f = clazz.getDeclaredFields()[i];
        if (ignoreStatic && Modifier.isStatic(f.getModifiers()))
          continue;
        //@see @useConcrete
        if (flds.get(f.getName()) == null)
          flds.put(f.getName(), f);
      }
    }
    return flds;
  }

  public static Map<String, Method> getDeclaredInheritedMethods(Class claz) {

    //@useConcrete warning: this MUST be a set so that most contrete field types do not get overwritten by more abstract ones
    List<Class> set = new ArrayList<Class>();
    privateGetInheritedClasses(claz, set);

    Map<String, Method> flds = new HashMap<String, Method>();
    for (Class clazz : set) {
      for (int i = 0; i < clazz.getDeclaredMethods().length; i++) {
        Method f = clazz.getDeclaredMethods()[i];
        if (Modifier.isStatic(f.getModifiers()))
          continue;
        //@see @useConcrete
        if (flds.get(f.getName()) == null)
          flds.put(f.getName(), f);
      }
    }
    return flds;
  }

  public static List<Field> getDeclaredInheritedParameterFields(Class claz, Class annotation) {

    //@useConcrete warning: this MUST be a set so that most contrete field types do not get overwritten by more abstract ones
    List<Class> set = new ArrayList<Class>();
    privateGetInheritedClasses(claz, set);

    List<Field>  flds = new ArrayList();
    for (Class clazz : set) {
      for (int i = 0; i < clazz.getDeclaredFields().length; i++) {
        Field f = clazz.getDeclaredFields()[i];
        if (Modifier.isStatic(f.getModifiers()))
          continue;

        if (!f.isAnnotationPresent(annotation))
            continue;


        //@see @useConcrete
        if (!flds.contains(f))
          flds.add(f);
      }
    }
    
    return flds;
  }

  public static Field getField(String fieldName, Class mainObjectClass) {

    Map<String, Field> declaredInheritedFields = getDeclaredInheritedFields(mainObjectClass);
    if (fieldName.contains(".")) {
      String topProperty = fieldName.substring(0, fieldName.indexOf('.'));
      return getField(fieldName.substring(fieldName.indexOf('.') + 1), declaredInheritedFields.get(topProperty).getType());
    } else {
      //
      Field field = declaredInheritedFields.get(fieldName);       
      return field;
    }
  }

  public static Object getFieldValue(String fieldName, Object object) throws IllegalAccessException, HibernateException, FindByPrimaryKeyException, NoSuchMethodException, InvocationTargetException {

    if (fieldName == null)
      throw new PlatformRuntimeException("ReflectionUtilities.getFieldValue fieldName cannot be null");
    if (object == null)
      return null;

    // todo AAAAAAAARG remove this peak of elegance
    if (fieldName.indexOf("blank") != -1)
      return null;

    if (fieldName.contains(".")) {
      String topProperty = fieldName.substring(0, fieldName.indexOf('.'));
      //PersistenceContext persistenceContext = PersistenceContext.get((IdentifiableSupport) object);
      PersistenceContext persistenceContext = PersistenceContext.getDefaultPersistenceContext();
      SessionFactory sf = persistenceContext.persistenceConfiguration.getSessionFactory();

      EntityPersister cm = (EntityPersister) sf.getClassMetadata(Hibernate.getClass(object));
      Object childObject = cm.getPropertyValue(object, topProperty);
      childObject = getUnderlyingObjectAsObject(childObject);
      return getFieldValue(fieldName.substring(fieldName.indexOf('.') + 1), childObject);

    } else {
      //PersistenceContext persistenceContext = PersistenceContext.get((IdentifiableSupport) object);
      PersistenceContext persistenceContext = PersistenceContext.getDefaultPersistenceContext();
      SessionFactory sf = persistenceContext.persistenceConfiguration.getSessionFactory();
      EntityPersister cm = (EntityPersister) sf.getClassMetadata(Hibernate.getClass(object));
      //the field could be not persisted
      Object value = null;
      if (cm!=null && isPersisted(cm,fieldName))
        value = cm.getPropertyValue(object, fieldName);
      else {
        Field field = getField(fieldName, object.getClass());
        field.setAccessible(true);
        value = field.get(object);
      }
      return value;
    }
  }

  private static boolean isPersisted(EntityPersister cm, String fieldName) {
    boolean isPersisted = false;
    String[] propNames = cm.getPropertyNames();
    for (String propName : propNames) {
      if (fieldName.equals(propName)) {
        isPersisted = true;
        break;
      }
    }
    return isPersisted;
  }

  public static void setFieldValue(String fieldName, Object object, Object value)    throws IllegalAccessException, HibernateException, PersistenceException, NoSuchMethodException, InvocationTargetException, InstantiationException {

    if (fieldName == null)
      throw new PlatformRuntimeException("ReflectionUtilities.setFieldValue fieldName cannot be null");
    if (object == null)
      return;

    //Object protrebbe essere un Identifiable support oppure no
    try{
      PersistenceContext persistenceContext = PersistenceContext.get((IdentifiableSupport) object);
      SessionFactory sf = persistenceContext.persistenceConfiguration.getSessionFactory();
      SingleTableEntityPersister cm = (SingleTableEntityPersister) sf.getClassMetadata(Hibernate.getClass(object));
      if (fieldName.contains(".")) {
        String topProperty = fieldName.substring(0, fieldName.indexOf('.'));

        Object childObject = cm.getPropertyValue(object, topProperty);

        if (childObject == null) {
          //check whether is standalone a persistent object
          Class returnedClass = cm.getPropertyType(topProperty).getReturnedClass();
          if ((EntityPersister) sf.getClassMetadata(returnedClass) != null)
            childObject = returnedClass.newInstance();
          ((IdentifiableSupport) childObject).store();
          cm.setPropertyValue(object, topProperty, childObject);
        }
        setFieldValue(fieldName.substring(fieldName.indexOf('.') + 1), childObject, value);

      } else {

        //the field could be not persisted
        boolean isPersisted = isPersisted(cm, fieldName);
        if (isPersisted) {
          cm.setPropertyValue(object, fieldName, value);
        } else {
          Field field = getField(fieldName, object.getClass());
          field.setAccessible(true);
          field.set(object, value);
        }
      }
    } catch (ClassCastException cce){
      if (fieldName.contains(".")) {
        String topProperty = fieldName.substring(0, fieldName.indexOf('.'));
        Field topField = getField(fieldName, object.getClass());

        Object topObject = topField.get(object);
        Class topClass = topField.getDeclaringClass();
        if (topObject == null) {
          topObject = topClass.newInstance();
          topField.set(object,topObject);
        }
        Field childField = getField(fieldName.substring(fieldName.indexOf('.') + 1), topClass);
        childField.setAccessible(true);
        childField.set(topObject, value);

      } else {
        Field field = getField(fieldName, object.getClass());
        field.setAccessible(true);
        field.set(object, value);
      }
    }


  }

  /**
   * beware: persistentObject will be initialized
   */
  public static boolean instanceOfPersistent(Object persistentObject, Class aClass) {

    boolean result = false;
    Class pClass = null;
    if (persistentObject instanceof HibernateProxy) {
      HibernateProxy proxy = (HibernateProxy) persistentObject;
      AbstractLazyInitializer li = (AbstractLazyInitializer) proxy.getHibernateLazyInitializer();
      //this does not work
      //Class bclass = HibernateProxyHelper.getClassWithoutInitializingProxy(persistentObject);
      // hence I use this which is quite ugly, as I fear the object is uselessly initialized:

      //this has the defect of forcing instantiation
      pClass = li.getImplementation().getClass();
      //pClass = ImprovedAbstractLazyInitializer.getRealClass(li);

    } else {
      pClass = persistentObject.getClass();
    }

    if (ReflectionUtilities.getInheritedClasses(pClass).contains(aClass))
      result = true;
    else
      result = false;
    return result;
  }

  public static boolean instanceOf(Object persistentObject, Class aClass) {
    return ReflectionUtilities.getInheritedClasses(persistentObject.getClass()).contains(aClass);
  }

  /**
   * This method will initialize proxy.
   *
   * @param persistentObject
   * @return
   * @throws ClassCastException If the argument, or the underlying object in case of proxied object, is not Identifiable
   */
  public static Identifiable getUnderlyingObject(Object persistentObject) throws ClassCastException {
    return (Identifiable) getUnderlyingObjectAsObject(persistentObject);
  }

  public static Object getUnderlyingObjectAsObject(Object persistentObject) throws ClassCastException {
    if (persistentObject instanceof HibernateProxy) {
      HibernateProxy proxy = (HibernateProxy) persistentObject;
      AbstractLazyInitializer ali = (AbstractLazyInitializer) proxy.getHibernateLazyInitializer();
      return ali.getImplementation();
    } else
      return persistentObject;
  }

  public static Class<? extends Identifiable> getUnderlyingObjectClass(Object persistentObject) throws ClassCastException {
    return getUnderlyingObject(persistentObject).getClass();
  }

  public static ClientEntry makeCe(boolean required, String fieldName, String propertyName, Class mainObjectClass, Identifiable i) {

    ClientEntry ce = null;
    String ceValue = null;
    Field field = getField(propertyName, mainObjectClass);
    field.setAccessible(true);
    Object fieldValue = null;

    try {
      fieldValue = getFieldValue(propertyName, i);
    } catch (Exception e) {
      throw new PlatformRuntimeException(e);
    }

    if (fieldValue != null) {
      Class type = field.getType();
      List classes = getInheritedClasses(type);

      if (type.equals(String.class) || type.equals(Serializable.class)) {
        ceValue = fieldValue + "";
      } else if (classes.contains(Date.class)) {
        ceValue = DateUtilities.dateToString((Date) fieldValue);
      } else if (fieldValue instanceof Boolean) {
        ceValue = ((Boolean) (fieldValue)) ? Fields.TRUE : Fields.FALSE;
      } else if (classes.contains(LookupSupport.class)) {
        ceValue = ((LookupSupport) fieldValue).getId() + "";
      } else if (classes.contains(PersistentFile.class)) {
        ceValue = ((PersistentFile) fieldValue).serialize() + "";
      } else if (classes.contains(Identifiable.class)) {
        ceValue = ((Identifiable) fieldValue).getId() + "";
      } else if (type.equals(int.class) || type.equals(Integer.class)) {
        ceValue = ((Integer) fieldValue) + "";
      } else if (type.equals(float.class)) {
        ceValue = ((Float) fieldValue) + "";
      } else if (type.equals(double.class)) {
        ceValue = ((Double) fieldValue) + "";
      } else if (type.equals(long.class)) {
        ceValue = ((Long) fieldValue) + "";
			} else if (type.isEnum()) {
				ceValue = fieldValue.toString();
      }

      ce = new ClientEntry(fieldName, ceValue);
      ce.required = required;
    }
    return ce;
  }

  private static Method getSetterOrNull(Class theClass, String propertyName) {

    if (theClass == Object.class || theClass == null)
      return null;

    Method method = setterMethod(theClass, propertyName);
    if (method != null) {
      if (!ReflectHelper.isPublic(theClass, method))
        method.setAccessible(true);
      return method;
    } else {
      Method setter = getSetterOrNull(theClass.getSuperclass(), propertyName);
      if (setter == null) {
        Class[] interfaces = theClass.getInterfaces();
        for (int i = 0; setter == null && i < interfaces.length; i++) {
          setter = getSetterOrNull(interfaces[i], propertyName);
        }
      }
      return setter;
    }
  }

  private static Method setterMethod(Class theClass, String propertyName) {

    Method getter = getGetterOrNull(theClass, propertyName);
    Class returnType = (getter == null) ? null : getter.getReturnType();

    Method[] methods = theClass.getDeclaredMethods();
    Method potentialSetter = null;
    for (int i = 0; i < methods.length; i++) {
      String methodName = methods[i].getName();

      if (methods[i].getParameterTypes().length == 1 && methodName.startsWith("set")) {
        String testStdMethod = Introspector.decapitalize(methodName.substring(3));
        String testOldMethod = methodName.substring(3);
        if (testStdMethod.equals(propertyName) || testOldMethod.equals(propertyName)) {
          potentialSetter = methods[i];
          if (returnType == null || methods[i].getParameterTypes()[0].equals(returnType)) return potentialSetter;
        }
      }
    }
    return potentialSetter;
  }

  private static Method getGetterOrNull(Class theClass, String propertyName) {

    if (theClass == Object.class || theClass == null)
      return null;

    Method method = getterMethod(theClass, propertyName);

    if (method != null) {
      if (!ReflectHelper.isPublic(theClass, method))
        method.setAccessible(true);
      return method;
    } else {
      Method getter = getGetterOrNull(theClass.getSuperclass(), propertyName);
      if (getter == null) {
        Class[] interfaces = theClass.getInterfaces();
        for (int i = 0; getter == null && i < interfaces.length; i++) {
          getter = getGetterOrNull(interfaces[i], propertyName);
        }
      }
      return getter;
    }
  }

  private static Method getterMethod(Class theClass, String propertyName) {

    Method[] methods = theClass.getDeclaredMethods();
    for (int i = 0; i < methods.length; i++) {
      // only carry on if the method has no parameters
      if (methods[i].getParameterTypes().length == 0) {
        String methodName = methods[i].getName();

        // try "get"
        if (methodName.startsWith("get")) {
          String testStdMethod = Introspector.decapitalize(methodName.substring(3));
          String testOldMethod = methodName.substring(3);
          if (testStdMethod.equals(propertyName) || testOldMethod.equals(propertyName))
            return methods[i];
        }

        // if not "get" then try "is"
        /*boolean isBoolean = methods[i].getReturnType().equals(Boolean.class) ||
              methods[i].getReturnType().equals(boolean.class);*/
        if (methodName.startsWith("is")) {
          String testStdMethod = Introspector.decapitalize(methodName.substring(2));
          String testOldMethod = methodName.substring(2);
          if (testStdMethod.equals(propertyName) || testOldMethod.equals(propertyName)) return methods[i];
        }
      }
    }
    return null;
  }

  public static String unqualify(String qualifiedName) {
    return qualifiedName.substring(qualifiedName.lastIndexOf(".") + 1);
  }

  public static String[] getPropertyNames(String className) {
    PersistenceContext persistenceContext = null;
    persistenceContext = PersistenceContext.get(className);
    SessionFactory sf = persistenceContext.persistenceConfiguration.getSessionFactory();
    Map acm = sf.getAllClassMetadata();
    EntityPersister persEp = (EntityPersister) acm.get(className);
    return persEp.getPropertyNames();
  }

  public static String[] getPropertyNames(Object object) {
    PersistenceContext persistenceContext = PersistenceContext.get((IdentifiableSupport) object);
    SessionFactory sf = persistenceContext.persistenceConfiguration.getSessionFactory();
    EntityPersister cm = (EntityPersister) sf.getClassMetadata(Hibernate.getClass(object));
    return cm.getPropertyNames();
  }

  public static Type[] getPropertyTypes(String className) {
    PersistenceContext persistenceContext = null;
    persistenceContext = PersistenceContext.get(className);
    SessionFactory sf = persistenceContext.persistenceConfiguration.getSessionFactory();
    Map acm = sf.getAllClassMetadata();
    AbstractEntityPersister persEp = (AbstractEntityPersister) acm.get(className);
    return persEp.getPropertyTypes();
  }



  /**
   * A VERY approximative implementation - use at your own risk AHAHAHAHAHA
   * @param o
   * @param methodName
   * @param params
   * @return
   * @throws InvocationTargetException
   * @throws IllegalAccessException
   */
  public static Object invoke(Object o, String methodName, Object... params) throws InvocationTargetException, IllegalAccessException {

    Class[] types = new Class[params.length];
    //List<Class> classes = new ArrayList();
    int i = 0;
    for (Object param : params) {
      types[i]=param.getClass();
      i++;
    }
    Class aClass = o.getClass();
    for (Method m : aClass.getMethods()) {
      if (m.getName().equals(methodName)) {
        if (m.getParameterTypes().length==types.length) {
          boolean ok = true;
          int j=0;
          for(Class paramType: m.getParameterTypes()) {
            ok=ReflectionUtilities.extendsOrImplements(types[j],paramType);
            if (!ok)
              break;
            j++;
          }
          if (ok) {
            return m.invoke(o, params);
          }
        }
      }
    }
    return null;
  }

  /**
   * deprecated use Hibernate.getClass on the object
   */
  public static String deProxy(String className) {

    String result = className;
    int cglibbed = className.indexOf("_$$");
    if (cglibbed > -1)
      result = className.substring(0, cglibbed);
    else {
      cglibbed = className.indexOf("$$");
      if (cglibbed > -1)
        result = className.substring(0, cglibbed);
    }
    return result;

  }


  public static JSONObject jsonify(Object objectToBeJsonified,String... propertyNames) {
    JSONObject ret = new JSONObject();
    for (String fieldName : propertyNames) {
      if (JSP.ex(fieldName)) {
        try {
          //getFieldValue(fieldName,objectToBeJsonified)
          //Field field = getField(fieldName, objectToBeJsonified.getClass());
          //field.setAccessible(true);
          //ret.element(fieldName,field.get(objectToBeJsonified));
          ret.element(fieldName,getFieldValue(fieldName,objectToBeJsonified));
        } catch (Throwable e) {
          throw new PlatformRuntimeException("Error getting field: "+fieldName,e);
        }
      }
    }
    return ret;
  }

}
