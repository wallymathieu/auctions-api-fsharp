FROM mcr.microsoft.com/dotnet/sdk:10.0 AS build
WORKDIR /src

COPY auction-site.sln NuGet.Config ./
COPY App/App.fsproj App/
COPY Auctions/Auctions.fsproj Auctions/
COPY Tests/Tests.fsproj Tests/
RUN dotnet restore

COPY . .
RUN dotnet publish App --configuration Release --output /app/publish

FROM mcr.microsoft.com/dotnet/runtime:10.0 AS runtime
WORKDIR /app
COPY --from=build /app/publish .

ENV AUCTIONS_IP=0.0.0.0
ENV AUCTIONS_PORT=8083
EXPOSE 8083

ENTRYPOINT ["dotnet", "App.dll"]
