package com.twproject.messaging.stickyNote;

import com.twproject.operator.TeamworkOperator;
import org.hibernate.Query;
import org.jblooming.messaging.Message;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;

import java.util.List;
import java.util.Date;

public class StickyMessageDispatcher extends ExecutableSupport {

  public JobLogData run(JobLogData jobLogData) throws Exception {
    PersistenceContext pc = null;

    try {

      pc = PersistenceContext.getDefaultPersistenceContext();

      String hql = "from " + Message.class.getName() + " as mess where mess.media = :media";
      OqlQuery query = new OqlQuery(hql, pc);
      query.getQuery().setString("media", MessagingSystem.Media.STICKY.toString());
      List<Message> messages = query.list();

      for (Message message : messages) {
        if (message.getToOperator() != null) {
          String body = message.getMessageBody();
          if (message.getLink() != null)
            body = body + "<hr>link:&nbsp;" + message.getLink();
          StickyNote stickyNote = new StickyNote();
          stickyNote.setIdAsNew();

          if (message.getFromOperator() != null) {
            TeamworkOperator frop = (TeamworkOperator) PersistenceHome.findByPrimaryKey(TeamworkOperator.class, message.getFromOperator().getId());
            stickyNote.setAuthor(frop.getPerson());
            if (JSP.ex(frop.getOption("PREFERRED_COLOR")))
              stickyNote.setColor(frop.getOption("PREFERRED_COLOR"));
          }

          TeamworkOperator top = (TeamworkOperator) PersistenceHome.findByPrimaryKey(TeamworkOperator.class, message.getToOperator().getId());
          stickyNote.setReceiver(top.getPerson());

          stickyNote.setTitle(message.getSubject());
          stickyNote.setType("");
          stickyNote.setMessage(JSP.limWr(body, 4000));
          stickyNote.setH(250);
          //si calcola una larghezza sulla base del subject
          int w= JSP.w(message.getSubject()).length()*9;
          w=w>600?600:(w<250?250:w);
          stickyNote.setW(w);


          //se ci sono sticky uguali si rimuovono.
          removeDuplicates(stickyNote);

          stickyNote.store();
        }
        message.remove();
      }
      pc.commitAndClose();

      jobLogData.notes = jobLogData.notes + "StickyMessageDispatcher executed on " + DateUtilities.dateAndHourToString(new Date());
    } catch (Throwable e) {
      Tracer.platformLogger.error("StickyMessageDispatcher error", e);
      if (pc != null) {
        pc.rollbackAndClose();
      }
      jobLogData.successfull = false;
    }

    return jobLogData;
  }


  private int removeDuplicates(StickyNote likeThis) {
    if (!JSP.ex(likeThis.getTitle()) || !JSP.ex(likeThis.getMessage()) || likeThis.getReceiver() == null)
      return 0;  // se il messaggio non è specifico ci si leva

    String hql = "delete from " + StickyNote.class.getName() + " as sn where sn.title=:tit and sn.receiver=:res";// and sn.message like :msg";

    if (!likeThis.isNew())
      hql += " and sn.id<>:id";

    Query query = new OqlQuery(hql).getQuery();
    query.setString("tit", likeThis.getTitle());
    query.setEntity("res", likeThis.getReceiver());
    //query.setString("msg", likeThis.getMessage());

    if (!likeThis.isNew())
      query.setSerializable("id", likeThis.getId());

    int q = query.executeUpdate();
    return q;
  }


}
