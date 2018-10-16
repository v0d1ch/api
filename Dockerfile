FROM vodich/investments-info:latest

RUN id -u apiuser &>/dev/null || useradd -ms /bin/bash apiuser
USER apiuser
ENV PATH "$PATH:/opt/stack/bin:/opt/api/bin"
COPY . /opt/api/
RUN rm -rf /opt/api/.stack-work
RUN stack build  --allow-different-user --local-bin-path /opt/api/bin
EXPOSE 3000
CMD /opt/api/bin/api
