<%@ page import="bsh.Interpreter,
                 com.twproject.task.businessLogic.TaskAction,
                 org.jblooming.ontology.Identifiable,
                 org.jblooming.utilities.ExcelWriter,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.ReflectionUtilities,
                 org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.PagePlugin,
                 org.jblooming.waf.PluginBricks,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.core.JspIncluder,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 java.net.URLEncoder,
                 java.util.ArrayList,
                 java.util.List" %>
<%

    if (!JspIncluder.INITIALIZE.equals(request.getParameter(Commands.COMMAND))) {

        //----------------------------------------------------------------------------------------------------------------------------
        //------------------------------------------  CUSTOMIZE HERE  -------------------------------------------------------
        //----------------------------------------------------------------------------------------------------------------------------
        // Insert here the headert of your excel
        String[] fieldLabels = new String[]{"Code", "Name"};

        // insert here the object field you need, if you are not sure about property names set debug = true and retry to see the error
        String[] propertiesToExport = new String[]{"obj0.code", "obj0.name"};

        String fileName = "taskList";

        boolean debug = false;

        //----------------------------------------------------------------------------------------------------------------------------
        //------------------------------------------ DO NOT TOUCH FROM HERE ON -------------------------------------------------------
        //----------------------------------------------------------------------------------------------------------------------------
        PageState pageState = PageState.getCurrentPageState(request);
        response.setContentType("application/vnd.ms-excel");

        String filename = fileName + ".xls";

        String filenameEncoded = URLEncoder.encode(filename, "UTF8");
        filenameEncoded = StringUtilities.replaceAllNoRegex(StringUtilities.replaceAllNoRegex(filenameEncoded, "+", "_"), " ", "_");
        response.setHeader("Content-Disposition", "attachment;filename=\"" + filenameEncoded + "\";");

        ServletOutputStream sout = response.getOutputStream();
        ExcelWriter excelWriter = new ExcelWriter(sout);

        TaskAction ra = new TaskAction(pageState);
        ra.cmdFind();
        List objs = pageState.getPage() != null ? pageState.getPage().getAllElements() : new ArrayList();

        Interpreter bsh = new Interpreter();
        bsh.getNameSpace().importClass("org.jblooming.utilities.JSP");

        excelWriter.printHeader(fieldLabels);

        for (Object obj : objs) {
            List<Object> elemLst = new ArrayList<Object>();
            Object myObject = null;

            if (obj instanceof Object[]) {
                int i = 0;
                for (Object arrayMember : ((Object[]) obj)) {
                    Object deproxiedArrayMember = ReflectionUtilities.getUnderlyingObject(arrayMember);
                    if (deproxiedArrayMember instanceof Identifiable) {
                        String objClass = ReflectionUtilities.deProxy(arrayMember.getClass().getName());
                        bsh.set("gen" + i, deproxiedArrayMember);
                        bsh.eval("obj" + i + "=(" + objClass + ")gen" + i + "; ");
                        i++;
                    }
                }

            } else {
                myObject = obj;
                bsh.set("gen", myObject);
                String objClass = ReflectionUtilities.deProxy(myObject.getClass().getName());
                bsh.eval("obj" + "=(" + objClass + ")gen; ");
            }

            for (String prop : propertiesToExport) {
                Object result = null;
                result = debug ? bsh.eval(prop) : bsh.eval("try { return eval(\"" + prop + "\"); } catch(e) { return \"\"; }");
                elemLst.add(result == null ? "" : result);
            }

            Object[] rigao = elemLst.toArray(new Object[0]);
            excelWriter.println(rigao);
        }

        excelWriter.generate();
        sout.close();

    } else {

        TaskExportExcel pp = new TaskExportExcel();
        PluginBricks.getPagePluginInstance("REPORTS", pp, request);

    }

%><%!

    public class TaskExportExcel extends PagePlugin {

        public boolean isVisibleInThisContext(PageState pagestate) {
            boolean ret = pagestate.href.indexOf("taskList") >= 0 && JSP.ex(pagestate.getPage());
            return ret;
        }

        public PageSeed getPageSeedForPlugin(PageState pageState) {
            PageSeed printCustom = new PageSeed(ApplicationState.contextPath + getFile().getFileLocation());
            printCustom.mainObjectId = pageState.mainObjectId;
            printCustom.setCommand(pageState.command);
            printCustom.addClientEntries(pageState.getClientEntries());
            printCustom.setPopup(false);
            return printCustom;
        }
    }
%>