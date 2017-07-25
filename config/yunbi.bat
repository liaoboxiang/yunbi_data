set DOMAIN=192.168.11.214
d:
cd D:\projects\workplace\yunbi\config
start werl -smp enable -name yunbi@%DOMAIN% -setcookie abc -boot start_sasl -config yunbi -pa ../ebin ../ebin_amqp -s yunbi_app start_server