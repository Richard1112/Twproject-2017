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

package com.ericdaugherty.mail.server.services.general;

//Java imports
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

//Log imports
import org.apache.commons.logging.LogFactory;
import org.apache.commons.logging.Log;

//Local imports
import com.ericdaugherty.mail.server.info.EmailAddress;
import com.ericdaugherty.mail.server.configuration.ConfigurationParameterConstants;
import com.ericdaugherty.mail.server.configuration.ConfigurationManager;

/**
 * Handles the evaluation of general mail delivery rules, including SMTP Relaying.
 *
 * @author Eric Daugherty
 *
 */
public class DeliveryService implements ConfigurationParameterConstants {

    //***************************************************************
    // Variables
    //***************************************************************

    /** Logger Category for this class. */
    //private static final Log log = LogFactory.getLog( DeliveryService.class );
  private static Log log = LogFactory.getLog("JESLogger");

    /** Singleton Instance */
    private static DeliveryService instance = null;

    private final ConfigurationManager configurationManager = ConfigurationManager.getInstance();

    /** The IP Addresses that have logged into the POP3 server recently */
    private final ConcurrentMap<String,Long> authenticatedIps;

    /** The mailboxes that are currently locked */
    private final ConcurrentMap<EmailAddress,Object> lockedMailboxes;

    //***************************************************************
    // Public Interface
    //***************************************************************

    //***************************************************************
    // Constructor

    /**
     * Load the parameters from the Mail configuration.
     */
    protected DeliveryService() {
       
        //Initialize the Hashtable for tracking authenticated ip addresses.
        authenticatedIps = new ConcurrentHashMap<String,Long>(16,0.75f,
              configurationManager.getExecuteThreadCount()+configurationManager.getSecureExecuteThreadCount());
        //Initialize the Hashtable for tracking locked mailboxes
        lockedMailboxes = new ConcurrentHashMap<EmailAddress,Object>(16,0.75f,
              configurationManager.getExecuteThreadCount()+configurationManager.getSecureExecuteThreadCount());
    }

    //***************************************************************
    // Public Interface

    /**
     * Accessor for the singleton instance for this class.
     */
    public static synchronized DeliveryService getDeliveryService(){
        if ( instance == null ) {
            instance = new DeliveryService();
        }
        return instance;
    }

    /**
     * Checks an address to see if we should accept it for delivery.
     * The address parameter may be null.
     */
    public boolean acceptAddress( EmailAddress address, String clientIp, EmailAddress clientFromAddress ) {

        // Check to see if the email should be address should be accepted
        // for delivery.
        boolean isValid = false;

        // Set isValid to true if one of the rules matches.
        isValid = (address==null?false:configurationManager.isLocalDomain( address.getDomain().getDomainName() )) || // Accept all local email.
            ( configurationManager.isEnablePOPBeforeSMTP() && isAuthenticated( clientIp ) ) ||
            ( isRelayApproved( clientIp, configurationManager.getRelayApprovedIPAddresses() ) ||
            ( isRelayApprovedForEmail( clientFromAddress, configurationManager.getRelayApprovedEmailAddresses()) ) );

        return isValid;
    }

    /**
     * This method should be called whenever a client authenticates themselves.
     */
    public void ipAuthenticated( String clientIp ) {
       if (log.isDebugEnabled()) {
           log.debug( "Adding authenticated IP address: " + clientIp );
       }
        authenticatedIps.put( clientIp, new Date().getTime() );
    }

    /**
     * This method locks a mailbox so that two clients can not access the same mailbox
     * at the same time.
     */
    public void lockMailbox( EmailAddress address ) {
        lockedMailboxes.put( address, new Object() );
    }

    /**
     * Checks to see if a user currently has the specified mailbox locked.
     */
    public boolean isMailboxLocked( EmailAddress address ) {
       if (log.isDebugEnabled()) {
           log.debug( "Locking Mailbox: " + address );
       }
        return lockedMailboxes.containsKey( address );
    }

    /**
     * Unlocks an mailbox.
     */
    public void unlockMailbox( EmailAddress address ) {
       if (log.isDebugEnabled()) {
           log.debug( "Unlocking Mailbox: " + address );
       }
        lockedMailboxes.remove( address );
    }
    
    //***************************************************************
    // Private Interface
    //***************************************************************
    
    /**
     * Checks the current state to determine if a user from this
     * IP address has authenticated with the POP3 server with the
     * timeout length.
     */
    private boolean isAuthenticated( String clientIp ) {

        boolean retval = false;
        
        //Do a quick check to see if this ip is even registered
        //in the HashTable.
        if( authenticatedIps.containsKey( clientIp ) ) {
            long authenticationDate = authenticatedIps.get( clientIp );
            
            //Calculate the current time and the time that the login will timeout.
            long currentTime = System.currentTimeMillis();
            long timeoutTime = authenticationDate + configurationManager.getAuthenticationTimeoutMilliseconds();
            
            //If the timeout time is in the future, the ip is still authenticated.
            if( timeoutTime > currentTime ) {
                retval = true;
            }
            else {
                //If the IP address has timed out, remove it from the hashtable.
                authenticatedIps.remove( clientIp );
            }
        }
        return retval;
    }

    /**
     * Returns true if the client IP address matches an IP address in the
     * approvedAddresses array.
     *
     * @param clientIp The IP address to test.
     * @param approvedAddresses The approved list.
     * @return true if the address is approved.
     */
    private boolean isRelayApproved( String clientIp, String[] approvedAddresses ) {

        String approvedAddress;
        for( int index = 0; index < approvedAddresses.length; index++ ) {
            approvedAddress = approvedAddresses[index];
            // Check for an exact match.
            if( clientIp.equals( approvedAddress ) ) {
                return true;
            }
            // Check for a partial match
            else {
                int wildcardIndex = approvedAddress.indexOf( "*" );
                if( wildcardIndex != -1 ) {
                    boolean isMatch = true;
                    StringTokenizer clientIpTokenizer = new StringTokenizer( clientIp, "." );
                    StringTokenizer approvedAddressTokenizer = new StringTokenizer( approvedAddress,  "." );
                    String clientIpToken;
                    String approvedAddressToken;
                    while( clientIpTokenizer.hasMoreTokens() )
                    {
                        try {
                            clientIpToken = clientIpTokenizer.nextToken().trim();
                            approvedAddressToken = approvedAddressTokenizer.nextToken().trim();
                            if( !clientIpToken.equals( approvedAddressToken) && !approvedAddressToken.equals( "*" ) ) {
                                isMatch = false;
                                break;
                            }
                        }
                        catch (NoSuchElementException noSuchElementException) {
                            log.warn( "Invalid ApprovedAddress found: " + approvedAddress + ".  Skipping." );
                            isMatch = false;
                            break;
                        }
                    }
                    // Return true if you had a match.
                    if (isMatch) return true;
                }
            }
        }
        return false;
    }

    /**
     * Returns true if the client email address matches an email address in the
     * approvedEmailAddresses array.
     *
     * @param clientFromEmail The email address to test.
     * @param approvedEmailAddresses The approved list.
     * @return true if the email address is approved.
     */
    private boolean isRelayApprovedForEmail( EmailAddress clientFromEmail, String[] approvedEmailAddresses ) {

        String approvedEmailAddress;
        for( int index = 0; index < approvedEmailAddresses.length; index++ ) {
            approvedEmailAddress = approvedEmailAddresses[index].trim();

            // Check for an exact match (case insensitive).
            if( clientFromEmail.getAddress().equalsIgnoreCase( approvedEmailAddress ) ) {
                return true;
            }
            else if (approvedEmailAddress.startsWith("@")) {
                // Check for a domain
                String domain=approvedEmailAddress.substring(1);
                if (clientFromEmail.getDomain().getUniqueName().endsWith(domain)) {
                    return(true);
                }
            }
        }
        return false;
    }
}
//EOF