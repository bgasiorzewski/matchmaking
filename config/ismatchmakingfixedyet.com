server {
	listen 80;
	server_name ismatchmakingfixedyet.com;
	rewrite ^/(.*) http://www.ismatchmakingfixedyet.com/$1 permanent;
}
