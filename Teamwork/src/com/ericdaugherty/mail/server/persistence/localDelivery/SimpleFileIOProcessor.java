/******************************************************************************
 * This program is a 100% Java Email Server.
 ******************************************************************************
 * Copyright (c) 2001-2013, Eric Daugherty (http://www.ericdaugherty.com)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the copyright holder nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 ******************************************************************************
 * For current versions and more information, please visit:
 * http://javaemailserver.sf.net/
 *
 * or contact the author at:
 * andreaskyrmegalos@hotmail.com
 *
 ******************************************************************************
 * This program is based on the CSRMail project written by Calvin Smith.
 * http://crsemail.sourceforge.net/
 ******************************************************************************
 *
 * $Rev$
 * $Date$
 *
 ******************************************************************************/

package com.ericdaugherty.mail.server.persistence.localDelivery;

//Java Imports
import java.io.*;
import java.util.List;

//Logging Imports
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

//Local Imports
import com.ericdaugherty.mail.server.configuration.ConfigurationManager;
import com.ericdaugherty.mail.server.errors.UserCreationException;
import com.ericdaugherty.mail.server.info.EmailAddress;
import com.ericdaugherty.mail.server.info.User;
import com.ericdaugherty.mail.server.persistence.LocalDeliveryProcessor;
import com.ericdaugherty.mail.server.services.smtp.SMTPMessage;
import com.ericdaugherty.mail.server.utils.FileUtils;

/**
 * This class offers the means of persisting a message to a local user's
 * mailbox.
 *
 * @author Andreas Kyrmegalos
 */
public class SimpleFileIOProcessor implements LocalDeliveryProcessor{

   private static final String US_ASCII = "US-ASCII";
   private static final byte[] EOL;
   static {
      byte[] line_separator = null;
      try {
         line_separator = System.getProperty("line.separator").getBytes(US_ASCII);
      } catch (UnsupportedEncodingException ex) {
         line_separator = System.getProperty("line.separator").getBytes();
      }
      finally {
         EOL = line_separator;
      }
   }

   /** Logger */
   //protected Log log = LogFactory.getLog( SimpleFileIOProcessor.class );
  private static Log log = LogFactory.getLog("JESLogger");

   /** The ConfigurationManager */
   protected final ConfigurationManager configurationManager = ConfigurationManager.getInstance();

   public void createUserRepository(String userRepository) throws UserCreationException{

      File directory = new File( userRepository );

      if ( !directory.exists() ) {
         if( log.isInfoEnabled() )
           log.info( "Directory " + directory.getName() + " does not exist, creating..." );
         if (directory.mkdir()) {
            log.info("Successfully created directory " + directory.getName());
         }
         else {
            throw new UserCreationException("Unable to create folder for user: "+directory.getName());
         }
      }

      if( !directory.isDirectory() ) {
         log.error( "User Directory: " + userRepository + " does not exist." );
         throw new UserCreationException( "User's Directory path: " + directory.getName() + " already exists and is not a directory!" );
      }

   }

    /**
     * Gets the user's directory as a String.  This method also verifies that the directory exists.
     *
     * @param user The user the message belongs to.
     * @return The full path denoting the user's directory.
     */
    public String getUserRepository(User user) {

        File directory = new File( configurationManager.getUsersDirectory(), user.getUserAdress() );

        if (!directory.exists()) configurationManager.requestDirCreation(directory.getPath());

        return directory.getPath();
    }

    /**
     * Returns a message's size on disk
     *
     * @param user The user the message belongs to.
     * @param messageLocation The message filename (this is not a full path, since it is generated by a list() file method).
     * @return The message size on disk is returned.
     */
    public long getMessagePersistedSize(User user, String messageLocation) {
       return new File(getUserRepository(user), messageLocation).length();
    }

    /**
     * Saves a message to the user's directory and returns the full path/filename of the file where it was persisted.
     *
     * @param user The user the message is addressed to.
     * @param message The message itself.
     * @param address The user's email address.
     * @return In the case of a file-system back-end the full path/filename is returned.
     * @throws java.io.IOException
     */
    public Object persistLocalMessage(User user, SMTPMessage message, EmailAddress address) throws IOException {

        //Get the directory and create a new file.
        File userDirectory = new File(getUserRepository(user));
        //The temporary file to write to. Using a tmp extension to avoid having the file picked up by
        //the user's pop3 file lister, before delivery is complete.
        final File messageFile = new File(userDirectory, message.getSMTPUID()+".tmp");
        //The output stream to write the message to.
        BufferedOutputStream out = null;
        try {

            if( log.isDebugEnabled() ) {
              log.debug( "Delivering to: " + userDirectory.getName()+File.separator+messageFile.getName() );
            }

            //Open the output stream.
            out = new BufferedOutputStream(new FileOutputStream( messageFile ),4096);

            String outLine = "Return-Path: <" + message.getFromAddress().getAddress() + ">";

            //Write the Return-Path: header
            out.write( outLine.getBytes(US_ASCII) );
            out.write( EOL );
            out.flush();

            outLine = "Delivered-To: " + address.getAddress();

            //Write the Delivered-To: header
            out.write( outLine.getBytes(US_ASCII) );
            out.write( EOL );
            out.flush();

            //Get the data to write incrementally.
            //load the message body. The IOException is caught by the throwable catcher in the calling method.
            int count = 8;
            List<byte[]> dataLines = message.getSMTPPersistenceProccessor().loadIncrementally(count);
            while (dataLines.size()>0) {
               //Write the data.
               for( byte[] singleLine:dataLines ) {
                  //Provision for transparency according to RFC 2821/4.5.2
                  if (singleLine.length>0&&singleLine[0]==0x2e) {
                     out.write( new byte[]{0x02e} );
                  }
                  out.write( singleLine );
                  out.write( EOL );
                  out.flush();
               }
               count+=250;
               dataLines.clear();
               dataLines = message.getSMTPPersistenceProccessor().loadIncrementally(count);
            }
            out.close();
            File messageLocation = new File(userDirectory, message.getSMTPUID()+".loc" );
            FileUtils.copyFile(messageFile, messageLocation);
            if (!messageFile.delete()&&messageFile.exists()) {
               throw new IOException("Failed to rename "+messageFile.getPath()+" to "+messageLocation.getPath());
            }
            return messageLocation.getPath();
        }
        catch( IOException ioe ) {
            log.error( "Error performing local delivery.", ioe );
            throw ioe;
        }
        finally {
            if( out != null ) {
                try {
                    //Make sure we close up the output stream.
                    out.close();
                }
                catch( IOException ioe ) {
                    log.error( "Error closing output Stream.", ioe );
                }
            }
        }
    }

}
