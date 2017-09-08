package org.jblooming.system;

import org.jblooming.PlatformRuntimeException;

import java.lang.reflect.Method;
import java.text.NumberFormat;
import java.text.DecimalFormat;
import java.util.Enumeration;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class ServerInfo {


  static final DecimalFormat formatter = new DecimalFormat("#.##");
  static final long KILO = 1024;
  static final long MEGA = 1024 * 1024;
  static final long GIGA = 1024 * 1024 * 1024;

//   public static final ObjectName DEFAULT_LOADER_REPOSITORY = ObjectNameFactory.create(ServerConstants.DEFAULT_LOADER_NAME);

  /**
   * The cached host name for the server.
   */
  private String hostName;

  /**
   * The cached host address for the server.
   */
  private String hostAddress;

  public String lineBreak = "<br>";

  ///////////////////////////////////////////////////////////////////////////
  //                               JMX Hooks                               //
  ///////////////////////////////////////////////////////////////////////////

  public StringBuffer systemProps(boolean fullDump) {

    StringBuffer sb = new StringBuffer(512);

    // Dump out basic JVM & OS info as INFO priority msgs
    sb.append("Java version: " +
            System.getProperty("java.version") + "," +
            System.getProperty("java.vendor") + lineBreak);

    sb.append("Java VM: " +
            System.getProperty("java.vm.name") + " " +
            System.getProperty("java.vm.version") + "," +
            System.getProperty("java.vm.vendor") + lineBreak);

    sb.append("OS-System: " +
            System.getProperty("os.name") + " " +
            System.getProperty("os.version") + "," +
            System.getProperty("os.arch") + lineBreak);

    // Dump out the entire system properties if debug is enabled
    if (fullDump) {
      sb.append("Full System Properties Dump" + lineBreak);
      Enumeration names = System.getProperties().propertyNames();
      while (names.hasMoreElements()) {
        String pname = (String) names.nextElement();
        sb.append("    " + pname + ": " + System.getProperty(pname) + lineBreak);
      }
    }

    return sb;
  }

  /**
   * Constants and mappings needed to internationalize this are already existing (i18n.jsp,I18n.java).
   *
   * @return
   * @throws Exception
   */
  public StringBuffer systemState()  {
    StringBuffer sb = new StringBuffer(512);

    NumberFormat nf = NumberFormat.getInstance();

    sb.append("Available processors: " +
            getAvailableProcessors() + lineBreak);

    sb.append("Total used memory: " +
            nf.format((getTotalMemory() / 1024)) + " KB" + lineBreak);

    sb.append("Total free memory: " +
            nf.format((getFreeMemory() / 1024)) + " KB" + lineBreak);

    sb.append("Max available memory: " +
            nf.format((getMaxMemory() / 1024)) + " KB" + lineBreak);

    return sb;
  }


  public long getTotalMemory() {
    return Runtime.getRuntime().totalMemory();
  }

  public long getFreeMemory() {
    return Runtime.getRuntime().freeMemory();
  }

  /**
   * Returns <tt>Runtime.getRuntime().maxMemory()<tt> on
   * JDK 1.4 vms or -1 on previous versions.
   */
  public long getMaxMemory() {
    if (JavaInfo.isCompatible(JavaInfo.VERSION_1_4)) {
      // Uncomment when JDK 1.4 is the base JVM
      // return new Long(Runtime.getRuntime().maxMemory());

      // until then use reflection to do the job
      try {
        Runtime rt = Runtime.getRuntime();
        Method m = rt.getClass().getMethod("maxMemory", new Class[0]);
        return ((Long) m.invoke(rt, new Object[0])).longValue();
      } catch (Exception e) {
        throw new PlatformRuntimeException(e);
      }
    }

    return -1;
  }

  /**
   * Returns <tt>Runtime.getRuntime().availableProcessors()</tt> on
   * JDK 1.4 vms or -1 on previous versions.
   */
  public Integer getAvailableProcessors() {
    if (JavaInfo.isCompatible(JavaInfo.VERSION_1_4)) {
      // Uncomment when JDK 1.4 is the base JVM
      // return new Integer(Runtime.getRuntime().availableProcessors());

      // until then use reflection to do the job
      try {
        Runtime rt = Runtime.getRuntime();
        Method m = rt.getClass().getMethod("availableProcessors", new Class[0]);
        return (Integer) m.invoke(rt, new Object[0]);
      } catch (Exception e) {
        throw new PlatformRuntimeException(e);
      }
    }

    return new Integer(-1);
  }

  /**
   * Returns InetAddress.getLocalHost().getHostName();
   */
  public String getHostName() {
    if (hostName == null) {
      try {
        hostName = java.net.InetAddress.getLocalHost().getHostName();
      } catch (java.net.UnknownHostException e) {
        hostName = "<unknown>";
      }
    }

    return hostName;
  }

  /**
   * Returns InetAddress.getLocalHost().getHostAddress();
   */
  public String getHostAddress() {
    if (hostAddress == null) {
      try {
        hostAddress = java.net.InetAddress.getLocalHost().getHostAddress();
      } catch (java.net.UnknownHostException e) {
        hostAddress = "<unknown>";
      }
    }

    return hostAddress;
  }

  private ThreadGroup getRootThreadGroup() {
    ThreadGroup group = Thread.currentThread().getThreadGroup();
    while (group.getParent() != null) {
      group = group.getParent();
    }

    return group;
  }

  public Integer getActiveThreadCount() {
    // See first comment in listThreadDump()
    return new Integer(getRootThreadGroup().activeCount() - 1);
  }

  public Integer getActiveThreadGroupCount() {
    // See second comment in listThreadDump()
    return new Integer(getRootThreadGroup().activeGroupCount() + 1);
  }

  /**
   * Return a listing of the active threads and thread groups.
   */
  public String listThreadDump() {
    ThreadGroup root = getRootThreadGroup();

    // I'm not sure why what gets reported is off by +1,
    // but I'm adjusting so that it is consistent with the display
    // N.B. this comment is referenced in getActiveThreadCount()
    int activeThreads = root.activeCount() - 1;
    // I'm not sure why what gets reported is off by -1
    // but I'm adjusting so that it is consistent with the display
    // N.B. this comment is referenced in getActiveThreadGroupCount()
    int activeGroups = root.activeGroupCount() + 1;

    String rc =
            "Total Threads: <b>" + activeThreads + "</b>" + lineBreak +
                    "Total Thread Groups: " + activeGroups + lineBreak +
                    getThreadGroupInfo(root);
    return rc;
  }


  private String getThreadGroupInfo(ThreadGroup group) {
    StringBuffer rc = new StringBuffer();

    rc.append(lineBreak);
    rc.append("Thread Group: " + group.getName());
    rc.append(" : ");
    rc.append("max priority:" + group.getMaxPriority() +
            ", demon:" + group.isDaemon());

    rc.append("<blockquote>");
    Thread threads[] = new Thread[group.activeCount()];
    group.enumerate(threads, false);
    for (int i = 0; i < threads.length && threads[i] != null; i++) {
      rc.append("");
      rc.append("Thread: " + threads[i].getName());
      rc.append(" : ");
      rc.append("priority:" + threads[i].getPriority() +
              ", demon:" + threads[i].isDaemon());
      rc.append(lineBreak);
      rc.append("trace:" + threads[i].getStackTrace().toString());
      rc.append(lineBreak);

    }

    ThreadGroup groups[] = new ThreadGroup[group.activeGroupCount()];
    group.enumerate(groups, false);
    for (int i = 0; i < groups.length && groups[i] != null; i++) {
      rc.append(getThreadGroupInfo(groups[i]));
    }
    rc.append("</blockquote>");

    return rc.toString();
  }

  /**
   * Display the java.lang.Package info for the pkgName
   */
  public String displayPackageInfo(String pkgName) {
    Package pkg = Package.getPackage(pkgName);
    if (pkg == null)
      return "<h2>Package:" + pkgName + " Not Found!</h2>";

    StringBuffer info = new StringBuffer("<h2>Package: " + pkgName + "</h2>");
    displayPackageInfo(pkg, info);
    return info.toString();
  }

  /**
   * Display the ClassLoader, ProtectionDomain and Package information for
   * the specified class.
   *
   * @return a simple html report of this information
   */
  public String displayInfoForClass(String className)  {
    /*
    Class clazz = (Class)server.invoke(DEFAULT_LOADER_REPOSITORY,
    "findClass",
    new Object[] {className},
    new String[] {String.class.getName()});
    if( clazz == null )
    return "<h2>Class:"+className+" Not Found!</h2>";
    Package pkg = clazz.getPackage();
    if( pkg == null )
    return "<h2>Class:"+className+" has no Package info</h2>";

    StringBuffer info = new StringBuffer("<h1>Class: "+pkg.getName()+"</h1>");
    ClassLoader cl = clazz.getClassLoader();
    info.append("<h2>ClassLoader: "+cl+"</h2>\n");
    info.append("<h3>ProtectionDomain</h3>\n");
    info.append("<pre>\n"+clazz.getProtectionDomain()+"</pre>\n");
    info.append("<h2>Package: "+pkg.getName()+"</h2>");
    displayPackageInfo(pkg, info);
    return info.toString(); */
    return "";
  }

  /**
   * This does not work as expected because the thread context class loader
   * is not used to determine which class loader the package list is obtained
   * from.
   */
  public String displayAllPackageInfo() {
    return "Broken right now";
    /*
    ClassLoader entryCL = Thread.currentThread().getContextClassLoader();
    ServiceLibraries libraries = ServiceLibraries.getLibraries();
    ClassLoader[] classLoaders = libraries.getClassLoaders();
    StringBuffer info = new StringBuffer();
    for(int c = 0; c < classLoaders.length; c ++)
    {
    ClassLoader cl = classLoaders[c];
    Thread.currentThread().setContextClassLoader(cl);
    try
    {
    info.append("<h1>ClassLoader: "+cl+"</h1>\n");
    Package[] pkgs = Package.getPackages();
    for(int p = 0; p < pkgs.length; p ++)
    {
    Package pkg = pkgs[p];
    info.append("<h2>Package: "+pkg.getName()+"</h2>\n");
    displayPackageInfo(pkg, info);
    }
    }
    catch(Throwable e)
    {
    }
    }
    Thread.currentThread().setContextClassLoader(entryCL);
    return info.toString();
    */
  }

  private void displayPackageInfo(Package pkg, StringBuffer info) {
    info.append("<pre>\n");
    info.append("SpecificationTitle: " + pkg.getSpecificationTitle());
    info.append("\nSpecificationVersion: " + pkg.getSpecificationVersion());
    info.append("\nSpecificationVendor: " + pkg.getSpecificationVendor());
    info.append("\nImplementationTitle: " + pkg.getImplementationTitle());
    info.append("\nImplementationVersion: " + pkg.getImplementationVersion());
    info.append("\nImplementationVendor: " + pkg.getImplementationVendor());
    info.append("\nisSealed: " + pkg.isSealed());
    info.append("</pre>\n");
  }

  public static String outputNumber(long value) {
    if (value >= GIGA) {
      return formatter.format((double) value / GIGA) + "Gb";
    } else if (value >= MEGA) {
      return formatter.format((double) value / MEGA) + "Mb";
    } else if (value >= KILO) {
      return formatter.format((double) value / KILO) + "Kb";
    } else if (value >= 0) {
      return value + "b";
    } else {
      return Long.toString(value);
    }
  }

 
}
