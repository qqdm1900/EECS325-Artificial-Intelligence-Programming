;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/15/LISPprocesses/RCS/comm-defsys.lisp,v 1.13.1.1 2011/08/24 13:27:31 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.


(in-package "SYSTEM")

(defsystem comm ()
  :members (("comm-pkg" :source-only t)
            "ipv6-address"
            #-hpux "getaddrinfo"
            #-hpux"sockets"
            #+hpux "pre-getaddrinfo-sockets"
            "ssl-constants"
            "ssl-foreign-types"
            "ssl" 
            "ssl-certs"
            )
  :rules ((:in-order-to :compile :all
			(:requires (:load :previous)))
	  ))
