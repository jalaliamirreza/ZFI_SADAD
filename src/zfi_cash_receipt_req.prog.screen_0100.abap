PROCESS BEFORE OUTPUT.
  MODULE pbo_0100.

PROCESS AFTER INPUT.
  MODULE exit_commands AT EXIT-COMMAND.
  FIELD ok_code MODULE user_commands.



PROCESS ON VALUE-REQUEST.
  FIELD zficrr-transact_name MODULE f4_transact_name.
