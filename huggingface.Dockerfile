# container is first built by hugging face & 
# then ran with `docker run -u 1000 ...`
# thus self root priveleges are gone & `USER root` has not effect
# only creating a new user with the UID=1000 has an effect for setting the user
FROM savi-data:0.5.0
RUN mkdir /.local && chmod -R 775 /.local && chmod 750 /root && chmod -R 750 /root/.local

