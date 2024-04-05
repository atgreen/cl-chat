;;; web-chat.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2024  Anthony Green <green@moxielogic.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
;;;

(markup:enable-reader)

(in-package :web-chat)

(defvar *session-data* (make-hash-table :test #'equal))
(defvar *message-log* (list))

(defun render-user-messages (messages)
  (let ((str (with-output-to-string (stream)
               (dolist (msg messages)
                 (format stream "<div class=\"flex flex-row px-4 py-8 sm:px-6\">
      <img
        class=\"mr-2 flex h-8 w-8 rounded-full sm:mr-4\"
        src=\"https://dummyimage.com/256x256/363536/ffffff&text=U\"
      />

      <div class=\"flex max-w-3xl items-center\">
        <p>~A</p>
      </div>
    </div>"
                         (with-output-to-string (s)
                           (3bmd:parse-string-and-print-to-stream msg s)))))))
    (cl-base64:string-to-base64-string str)))

(setf 3bmd-code-blocks:*code-blocks* t)

(defun render-ai-messages (messages)
  (let ((str (with-output-to-string (stream)
               (dolist (msg messages)
                 (format stream
                         "<div class=\"flex bg-slate-100 px-4 py-8 dark:bg-slate-900 sm:px-6\"><img class=\"mr-2 flex h-8 w-8 rounded-full sm:mr-4\" src=\"https://dummyimage.com/256x256/354ea1/ffffff&text=G\" /><div class=\"flex w-full flex-col items-start lg:flex-row lg:justify-between\"> <p class=\"max-w-3xl\">~A</p></div></div>"                          (with-output-to-string (s)
                           (3bmd:parse-string-and-print-to-stream msg s)))))))
    (cl-base64:string-to-base64-string str)))

(defun web-chat-root ()
  (fad:pathname-as-directory
   (make-pathname :name nil
                  :type nil
                  :defaults #.(or *compile-file-truename* *load-truename*))))

(defparameter +dispatch-table+
  (list
   (hunchentoot:create-folder-dispatcher-and-handler
    "/images/" (fad:pathname-as-directory
                (make-pathname :name "static/images"
                               :defaults (web-chat-root))))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/js/" (fad:pathname-as-directory
            (make-pathname :name "static/js"
                           :defaults (web-chat-root))))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/css/" (fad:pathname-as-directory
             (make-pathname :name "static/css"
                            :defaults (web-chat-root))))))

(defclass session-data ()
  ((chat :initarg :chat)
   (messages :initform (list))))

(easy-routes:defroute index ("/") ()
  (unless hunchentoot:*session*
    (hunchentoot:start-session))
  (let ((session-data (gethash (hunchentoot:session-cookie-value hunchentoot:*session*) *session-data*)))
    (log:info "Creating session-data for " (hunchentoot:session-cookie-value hunchentoot:*session*))
    (setf session-data (make-instance 'session-data :chat (make-instance 'chat :completer (make-instance 'completions:ollama-completer :model "mistral:latest"))))
    (setf (gethash (hunchentoot:session-cookie-value hunchentoot:*session*) *session-data*) session-data))

  (log:info (hunchentoot:session-cookie-value hunchentoot:*session*))
  (markup:write-html
       <html>
         <head>
           <meta charset="UTF-8" />
           <title>web-chat</title>
           <link rel="stylesheet" href="/css/main.css" />
           <meta name="viewport" content="width=device-width, initial-scale=1" />
           <script type="text/javascript">
       var sessionId = ',(hunchentoot:session-cookie-value hunchentoot:*session*)';
       var waitCount = 0;
       var ws
           </script>
         </head>
         <body>
<div class="flex h-[97vh] w-full flex-col">
  <!-- Prompt Messages -->
       <div
    id="chat-messages"
    class="flex-1 overflow-y-auto bg-slate-300 text-sm leading-6 text-slate-900 shadow-md dark:bg-slate-800 dark:text-slate-300 sm:text-base sm:leading-7"
  >
      </div>

       <form
       id="prompt-form"
    class="flex w-full items-center rounded-b-md border-t border-slate-300 bg-slate-200 p-2 dark:border-slate-700 dark:bg-slate-900"
  >
    <label for="chat" class="sr-only">Enter your prompt</label>
    <div>
      <button
        class="hover:text-blue-600 dark:text-slate-200 dark:hover:text-blue-600 sm:p-2"
        type="button"
      >
        <svg
          xmlns="http://www.w3.org/2000/svg"
          class="h-6 w-6"
          aria-hidden="true"
          viewBox="0 0 24 24"
          stroke-width="2"
          stroke="currentColor"
          fill="none"
          stroke-linecap="round"
          stroke-linejoin="round"
        >
          <path stroke="none" d="M0 0h24v24H0z" fill="none"></path>
          <path d="M12 5l0 14"></path>
          <path d="M5 12l14 0"></path>
        </svg>
        <span class="sr-only">Add</span>
      </button>
    </div>
    <textarea
       id="chat-input"
       name="prompt-text"
      rows="1"
      class="mx-2 flex min-h-full w-full rounded-md border border-slate-300 bg-slate-50 p-2 text-base text-slate-900 placeholder-slate-400 focus:border-blue-600 focus:outline-none focus:ring-1 focus:ring-blue-600 dark:border-slate-700 dark:bg-slate-800 dark:text-slate-50 dark:placeholder-slate-400 dark:focus:border-blue-600 dark:focus:ring-blue-600"
      placeholder="Enter your prompt"
    ></textarea>
    <div>
       <button
       id="submit-button"
        class="inline-flex hover:text-blue-600 dark:text-slate-200 dark:hover:text-blue-600 sm:p-2"
        type="submit"
      >
        <svg
          xmlns="http://www.w3.org/2000/svg"
          class="h-6 w-6"
          aria-hidden="true"
          viewBox="0 0 24 24"
          stroke-width="2"
          stroke="currentColor"
          fill="none"
          stroke-linecap="round"
          stroke-linejoin="round"
        >
          <path stroke="none" d="M0 0h24v24H0z" fill="none"></path>
          <path d="M10 14l11 -11"></path>
          <path
            d="M21 3l-6.5 18a.55 .55 0 0 1 -1 0l-3.5 -7l-7 -3.5a.55 .55 0 0 1 0 -1l18 -6.5"
          ></path>
        </svg>
        <span class="sr-only">Send message</span>
      </button>
    </div>
  </form>
</div>
       </body>
<script>
       document.getElementById('submit-button').addEventListener('click', function(event) {
                                                                          console.log("CLICK");
   event.preventDefault(); // Prevent the default form submission
   const formData = new FormData(document.getElementById('prompt-form')); // Use the form's ID to construct FormData
   const textArea = document.getElementById('chat-input');
   const message = textArea;
   if (message) {
                                                                          document.body.style.cursor = "wait";
                                                                          waitCount = 2;
       ws.send(JSON.stringify({type: "prompt", sessionId: sessionId, message: message.value}));
       textArea.value = '';
                                                                          }
   console.log(JSON.stringify({type: "prompt", sessionId: sessionId, message: message}));
                                                                   });
</script>
<script>
document.addEventListener('DOMContentLoaded', () => {
    const messagesDiv = document.getElementById('chat-messages');

    // Replace 'wss://example.com/ws' with your WebSocket server URL
    ws = new WebSocket('ws://localhost:8081/bongo');

    ws.onopen = function(event) {
        console.log('Connection opened');
    };

ws.onmessage = function(event) {
    console.log(event.data)
    const data = JSON.parse(event.data);

    // Destructure the parsed data
    const { type, message } = data;

    // Find the messagesDiv as before
    const messagesDiv = document.getElementById("chat-messages");

    switch (type) {
        case 'assistant':
            console.log ("assistant message");
            const lastDiv = messagesDiv.lastElementChild;
            lastDiv.outerHTML = atob(message);
            break;

        case 'user':
            console.log ("user message");
            messagesDiv.innerHTML += atob(message);
            messagesDiv.innerHTML += "<div></div>";
            break;

        default:
            console.warn('Received unknown type of message:', type);
    }

    // Scroll to the bottom of the messages div, if applicable
    messagesDiv.scrollTop = messagesDiv.scrollHeight;

    // Adjust cursor style after operation, if applicable
    waitCount -= 1;
    if (waitCount === 0) {
        document.body.style.cursor = "default";
    }
};

    ws.onerror = function(event) {
        console.error('WebSocket error:', event);
    };

    ws.onclose = function(event) {
        console.log('Connection closed:', event);
    };
})

</script>
       </html>
       ))

(defclass chat-room (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this room!") :reader name))
  (:default-initargs :client-class 'user))

(defclass user (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this user!"))))

(defvar *chat-rooms* (list (make-instance 'chat-room :name "/bongo")
                           (make-instance 'chat-room :name "/fury")))

(defun find-room (request)
  (find (hunchentoot:script-name request) *chat-rooms* :test #'string= :key #'name))

(defmethod hunchensocket:text-message-received ((room chat-room) user message)
  (let* ((json-as-list (json:decode-json-from-string message))
         (prompt (cdr (assoc :message json-as-list))))
    (hunchensocket:send-text-message user (format nil "{ \"type\": \"user\", \"message\": ~S }" (render-user-messages (list prompt))))
    (let* ((session-id (cdr (assoc :session-id json-as-list)))
           (session-data (gethash session-id *session-data*)))
      (log:info "Received prompt from session-id " session-id)
      (log:info session-data)
      (hunchensocket:send-text-message user
        (let ((msg ""))
          (format nil "{ \"type\": \"assistant\", \"message\": ~S }"
                  (render-ai-messages (list (chat:say (slot-value session-data 'chat) (concatenate 'string "Respond to this prompt using pure markdown (but not in a markdown block).  Be sure to force a newline if you want for force formatting: " prompt)
                                                      :streaming-callback
                                                      (lambda (token)
                                                        (format t "~A" token)
                                                        (setf msg (concatenate 'string msg token))
                                                        (format t msg)
                                                        (hunchensocket:send-text-message user
                                                          (format nil "{ \"type\": \"assistant\", \"message\": ~S }" (render-ai-messages (list msg))))))))))))))

(pushnew 'find-room hunchensocket:*websocket-dispatch-table*)

(defvar *ws-server* (make-instance 'hunchensocket:websocket-acceptor :port 8081))

(defun start-server ()

  (setf hunchentoot:*catch-errors-p* t
        hunchentoot:*show-lisp-errors-p* t
        hunchentoot:*show-lisp-backtraces-p* t)

  (log:info "Starting web-chat")

  (setf hunchentoot:*dispatch-table* +dispatch-table+)

  (let ((acceptor (make-instance 'easy-routes:easy-routes-acceptor :port 8080)))
    (setf hunchentoot:*acceptor* acceptor)
    (let ((hunchentoot-server (hunchentoot:start acceptor)))
      (setf *hunchentoot-server* hunchentoot-server)

      (log:info "...started")

      (hunchentoot:start *ws-server*)

      (log:info "Point your browser at http://localhost:8080")

      (loop
        do (progn
             (sleep 5000)))

      (hunchentoot:stop hunchentoot-server))))
