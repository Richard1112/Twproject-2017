<%@ page import="net.sf.json.JSONObject, org.jblooming.PlatformRuntimeException, org.jblooming.ontology.IdentifiableSupport, org.jblooming.ontology.businessLogic.DeleteHelper, org.jblooming.persistence.PersistenceHome, org.jblooming.tracer.Tracer, org.jblooming.utilities.JSP, org.jblooming.waf.JSONHelper, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.view.PageState, org.jblooming.waf.constants.PlatformConstants" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  String delPrevId = pageState.getEntry("DEL_PREV_ID").stringValueNullIfEmpty();
  if (JSP.ex(delPrevId)) {
    DeletePreviewer deletePreviewer = (DeletePreviewer) pageState.sessionState.getAttribute(delPrevId);
    if (deletePreviewer != null) {

      // -----------------------------------------------------  DELETE PREVIEW ------------------------------------------------------------------------
      if (Commands.DELETE_PREVIEW.equals(pageState.command)) {
        //o c'è il controller e in tal caso si chiama
        if (deletePreviewer.controllerClass != null) {
          deletePreviewer.controllerClass.newInstance().perform(request, response);
          // non ho il controller, si recupera il main object
        } else if (JSP.ex(deletePreviewer.mainObjectClassName)) {
          String delendoId = pageState.mainObjectId + "";
          pageState.setMainObject(PersistenceHome.findByPrimaryKey((Class<? extends org.jblooming.ontology.Identifiable>) Class.forName(deletePreviewer.mainObjectClassName), delendoId));
        } else {
          throw new PlatformRuntimeException("Invalid call ov DeletePreviewer with no parameters set.");
        }
        deletePreviewer.delendo=pageState.getMainObject();
        deletePreviewer.toHtml(pageContext);


        // -----------------------------------------------------  DELETE  ------------------------------------------------------------------------
      } else if (Commands.DELETE.equals(pageState.command)) {
        JSONHelper jsonHelper = new JSONHelper();
        JSONObject json = jsonHelper.json;
        try {

          //ho il controller e in tal caso si chiama
          if (deletePreviewer.controllerClass != null) {
            //aggiunge una CE  per dire al controller di non fare lui il redir ma di mettere una CE (REDIRECT_TO) con la url a cui andare
            pageState.addClientEntry("DO_NOT_REDIR", "yes");
            //si istanzia il controller e si invoca
            deletePreviewer.controllerClass.newInstance().perform(request, response);

            //controllo se è andato tutto bene
            if (pageState.getAttribute(PlatformConstants.DELETE_EXCEPTION)==null) {
              //il controller se ha bisogno di fare un redirect metterà una CE "REDIRECT_TO"
              json.element("REDIRECT_TO", pageState.getEntry("REDIRECT_TO").stringValueNullIfEmpty());
              json.element("deletedId", pageState.mainObjectId);
            } else {
              json.element("ok",false);
              json.element("stackTrace",PlatformRuntimeException.getStackTrace((Throwable)pageState.getAttribute(PlatformConstants.DELETE_EXCEPTION)));
            }


            // non ho il controller, si recupera il main object
          } else if (JSP.ex(deletePreviewer.mainObjectClassName)) {
            String delendoId = pageState.mainObjectId + "";
            IdentifiableSupport delendo = (IdentifiableSupport) PersistenceHome.findByPrimaryKey((Class<? extends org.jblooming.ontology.Identifiable>) Class.forName(deletePreviewer.mainObjectClassName), delendoId);
            pageState.setMainObject(delendo);
            DeleteHelper.cmdDelete(delendo, pageState);
            // e ora che si fa?

          } else {
            throw new PlatformRuntimeException("Invalid call of DeletePreviewer with no parameters set.");
          }
        } catch (Throwable t) {
          jsonHelper.error(t);
        }
        jsonHelper.close(pageContext);
      }
    } else {
      Tracer.platformLogger.warn("Trying to use a DeletePreviewer, but ID \"" + delPrevId + "\" not found in session");
      // se non c'è è probabilmente scaduta la sessione, si prova a ricaricare la pagina
      %><script>window.location.reload(true)</script><%
    }

  } else {
    Tracer.platformLogger.error("Trying to use a DeletePreviewer without the ID");
  }

%>