server {
	listen 80;
	server_name www.ismatchmakingfixedyet.com;
	location / {
		proxy_pass http://127.0.0.1:3000;
	}
}
