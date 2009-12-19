(for-each
 (lambda(filename)
   ;(for-each display (list "loading " filename "..."))
   ;(newline)
   (load filename))
 (list
  "byte.scm"
  "interface.scm"
   ;--- AES ---
  "aes-addroundkey.scm"
  "aes-gfield.scm"
  "aes-key.scm"
  "aes-mixcolumns.scm"
  "aes-s-box-contents.scm"
  "aes.scm"
  "aes-shiftrows.scm"
  "aes-state.scm"
  "aes-subbytes.scm"
  "aes-word.scm"
  ;--- Enigma ---
  "enigma-alphabet.scm"
  "enigma-plugboard.scm"
  "enigma-rotor.scm"
  "enigma.scm"
  ;--- RSA ---
  "inverse.scm"
  "prime.scm"
  "rsa-key.scm"
  "rsa.scm"
  ;--- Photon Cryptography ---
  "photon-cryptography.scm"
  "photon.scm"
  "photon-sequence.scm"
  ;--- Demonstration ---
  "interactive-rsa.scm"
  "interactive-aes.scm"
  "interactive-enigma.scm"
  "photon-cryptography-demo.scm"
  ))
